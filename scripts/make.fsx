﻿#!/usr/bin/env fsharpi

open System
open System.IO
open System.Linq
open System.Diagnostics

#r "System.Configuration"
#load "InfraLib/Misc.fs"
#load "InfraLib/Process.fs"
#load "InfraLib/Git.fs"
open FSX.Infrastructure
open Process


let UNIX_NAME = "gwallet"
let CONSOLE_FRONTEND = "GWallet.Frontend.Console"
let GTK_FRONTEND = "GWallet.Frontend.XF.Gtk"
let DEFAULT_SOLUTION_FILE = "gwallet.core.sln"
let LINUX_SOLUTION_FILE = "gwallet.linux.sln"

type Frontend =
    | Console
    | Gtk
    member self.GetProjectName() =
        match self with
        | Console -> CONSOLE_FRONTEND
        | Gtk -> GTK_FRONTEND
    override self.ToString() =
        sprintf "%A" self

type BinaryConfig =
    | Debug
    | Release
    override self.ToString() =
        sprintf "%A" self

let rec private GatherTarget (args: string list, targetSet: Option<string>): Option<string> =
    match args with
    | [] -> targetSet
    | head::tail ->
        if (targetSet.IsSome) then
            failwith "only one target can be passed to make"
        GatherTarget (tail, Some (head))

let buildConfigContents =
    let buildConfig = FileInfo (Path.Combine (__SOURCE_DIRECTORY__, "build.config"))
    if not (buildConfig.Exists) then
        Console.Error.WriteLine "ERROR: configure hasn't been run yet, run ./configure.sh first"
        Environment.Exit 1

    let skipBlankLines line = not <| String.IsNullOrWhiteSpace line
    let splitLineIntoKeyValueTuple (line:string) =
        let pair = line.Split([|'='|], StringSplitOptions.RemoveEmptyEntries)
        if pair.Length <> 2 then
            failwith "All lines in build.config must conform to format:\n\tkey=value"
        pair.[0], pair.[1]

    let buildConfigContents =
        File.ReadAllLines buildConfig.FullName
        |> Array.filter skipBlankLines
        |> Array.map splitLineIntoKeyValueTuple
        |> Map.ofArray
    buildConfigContents

let GetOrExplain key map =
    match map |> Map.tryFind key with
    | Some k -> k
    | None   -> failwithf "No entry exists in build.config with a key '%s'." key

let prefix = buildConfigContents |> GetOrExplain "Prefix"
let libInstallDir = DirectoryInfo (Path.Combine (prefix, "lib", UNIX_NAME))
let binInstallDir = DirectoryInfo (Path.Combine (prefix, "bin"))

let wrapperScript = """#!/bin/sh
set -e
exec mono "$TARGET_DIR/$GWALLET_PROJECT.exe" "$@"
"""

let rootDir = DirectoryInfo(Path.Combine(__SOURCE_DIRECTORY__, ".."))

let PrintNugetVersion () =
    let nugetExe = Path.Combine(rootDir.FullName, ".nuget", "nuget.exe") |> FileInfo
    if not (nugetExe.Exists) then
        false
    else
        let nugetProc = Process.Execute ({ Command = "mono"; Arguments = nugetExe.FullName }, Echo.Off)
        Console.WriteLine nugetProc.Output.StdOut
        if nugetProc.ExitCode = 0 then
            true
        else
            Console.Error.WriteLine nugetProc.Output.StdErr
            Console.WriteLine()
            failwith "nuget process' output contained errors ^"

let BuildSolution buildTool solutionFileName binaryConfig extraOptions =

    let configOption = sprintf "/p:Configuration=%s" (binaryConfig.ToString())
    let configOptions =
        match buildConfigContents |> Map.tryFind "DefineConstants" with
        | Some constants -> sprintf "%s;DefineConstants=%s" configOption constants
        | None   -> configOption
    let buildArgs = sprintf "%s %s %s"
                            solutionFileName
                            configOptions
                            extraOptions
    let buildProcess = Process.Execute ({ Command = buildTool; Arguments = buildArgs }, Echo.All)
    if (buildProcess.ExitCode <> 0) then
        Console.Error.WriteLine (sprintf "%s build failed" buildTool)
        PrintNugetVersion() |> ignore
        Environment.Exit 1

let JustBuild binaryConfig: Frontend*FileInfo =
    printfn "Building in %s mode..." (binaryConfig.ToString().ToUpper())
    let buildTool = Map.tryFind "BuildTool" buildConfigContents
    if buildTool.IsNone then
        failwith "A BuildTool should have been chosen by the configure script, please report this bug"

    BuildSolution buildTool.Value DEFAULT_SOLUTION_FILE binaryConfig String.Empty

    let frontend =
        // older mono versions (which only have xbuild, not msbuild) can't compile .NET Standard assemblies
        if buildTool.Value = "msbuild" && Misc.GuessPlatform () = Misc.Platform.Linux then

            let pkgConfigForGtkProc = Process.Execute({ Command = "pkg-config"; Arguments = "gtk-sharp-2.0" }, Echo.All)
            let isGtkPresent =
                (0 = pkgConfigForGtkProc.ExitCode)

            if isGtkPresent then

                // somehow, msbuild doesn't restore the dependencies of the GTK frontend (Xamarin.Forms in particular)
                // when targetting the LINUX_SOLUTION_FILE below, so we need this workaround. TODO: report this bug
                let nugetWorkaroundArgs =
                    sprintf ".nuget/nuget.exe restore src/%s/%s.fsproj -SolutionDirectory ." GTK_FRONTEND GTK_FRONTEND
                Process.Execute({ Command = "mono"; Arguments = nugetWorkaroundArgs }, Echo.All) |> ignore

                BuildSolution "msbuild" LINUX_SOLUTION_FILE binaryConfig "/t:Restore"
                // TODO: report as a bug the fact that /t:Restore;Build doesn't work while /t:Restore and later /t:Build does
                BuildSolution "msbuild" LINUX_SOLUTION_FILE binaryConfig "/t:Build"
                Frontend.Gtk
            else
                Frontend.Console
        else
            Frontend.Console

    let scriptName = sprintf "%s-%s" UNIX_NAME (frontend.ToString().ToLower())
    let launcherScriptFile = FileInfo (Path.Combine (__SOURCE_DIRECTORY__, "bin", scriptName))
    Directory.CreateDirectory(launcherScriptFile.Directory.FullName) |> ignore
    let wrapperScriptWithPaths =
        wrapperScript.Replace("$TARGET_DIR", libInstallDir.FullName)
                     .Replace("$GWALLET_PROJECT", frontend.GetProjectName())
    File.WriteAllText (launcherScriptFile.FullName, wrapperScriptWithPaths)
    frontend,launcherScriptFile

let MakeCheckCommand (commandName: string) =
    if not (Process.CommandWorksInShell commandName) then
        Console.Error.WriteLine (sprintf "%s not found, please install it first" commandName)
        Environment.Exit 1

let GetPathToFrontend (frontend: Frontend) (binaryConfig: BinaryConfig): DirectoryInfo*FileInfo =
    let frontendProjName = frontend.GetProjectName()
    let dir = Path.Combine ("src", frontendProjName, "bin", binaryConfig.ToString()) |> DirectoryInfo
    let mainExecFile = Path.Combine(dir.FullName, frontendProjName + ".exe") |> FileInfo
    dir,mainExecFile

let maybeTarget = GatherTarget (Misc.FsxArguments(), None)
match maybeTarget with
| None ->
    JustBuild BinaryConfig.Debug
        |> ignore

| Some("release") ->
    JustBuild BinaryConfig.Release
        |> ignore

| Some "nuget" ->
    Console.WriteLine "This target is for debugging purposes."

    if not (PrintNugetVersion()) then
        Console.Error.WriteLine "Nuget executable has not been downloaded yet, try `make` alone first"
        Environment.Exit 1

| Some("zip") ->
    let zipCommand = "zip"
    MakeCheckCommand zipCommand

    let version = Misc.GetCurrentVersion(rootDir).ToString()

    let release = BinaryConfig.Release
    let frontend,script = JustBuild release
    let binDir = "bin"
    Directory.CreateDirectory(binDir) |> ignore

    let zipNameWithoutExtension = sprintf "%s-v%s" script.Name version
    let zipName = sprintf "%s.zip" zipNameWithoutExtension
    let pathToZip = Path.Combine(binDir, zipName)
    if (File.Exists (pathToZip)) then
        File.Delete (pathToZip)

    let pathToFolderToBeZipped = Path.Combine(binDir, zipNameWithoutExtension)
    if (Directory.Exists (pathToFolderToBeZipped)) then
        Directory.Delete (pathToFolderToBeZipped, true)

    let pathToFrontend,_ = GetPathToFrontend frontend release
    let zipRun = Process.Execute({ Command = "cp"
                                   Arguments = sprintf "-rfvp %s %s" pathToFrontend.FullName pathToFolderToBeZipped },
                                 Echo.All)
    if (zipRun.ExitCode <> 0) then
        Console.Error.WriteLine "Precopy for ZIP compression failed"
        Environment.Exit 1

    let previousCurrentDir = Directory.GetCurrentDirectory()
    Directory.SetCurrentDirectory binDir
    let zipLaunch = { Command = zipCommand
                      Arguments = sprintf "%s -r %s %s"
                                      zipCommand zipName zipNameWithoutExtension }
    let zipRun = Process.Execute(zipLaunch, Echo.All)
    if (zipRun.ExitCode <> 0) then
        Console.Error.WriteLine "ZIP compression failed"
        Environment.Exit 1
    Directory.SetCurrentDirectory previousCurrentDir

| Some("check") ->
    Console.WriteLine "Running tests..."
    Console.WriteLine ()

    let nunitCommand = "nunit-console"
    MakeCheckCommand nunitCommand
    let testAssembly = "GWallet.Backend.Tests"
    let testAssemblyPath = Path.Combine(__SOURCE_DIRECTORY__, "..", "src", testAssembly, "bin",
                                        testAssembly + ".dll")
    if not (File.Exists(testAssemblyPath)) then
        failwithf "File not found: %s" testAssemblyPath
    let nunitRun = Process.Execute({ Command = nunitCommand; Arguments = testAssemblyPath },
                                   Echo.All)
    if (nunitRun.ExitCode <> 0) then
        Console.Error.WriteLine "Tests failed"
        Environment.Exit 1

| Some("install") ->
    let frontend,launcherScript = JustBuild BinaryConfig.Release

    let mainBinariesDir = DirectoryInfo (Path.Combine(__SOURCE_DIRECTORY__, "..",
                                                       "src", frontend.GetProjectName(), "bin", "Release"))

    Console.WriteLine "Installing..."
    Console.WriteLine ()

    Misc.CopyDirectoryRecursively (mainBinariesDir, libInstallDir, [])

    let finalLauncherScriptInPrefix = FileInfo (Path.Combine(binInstallDir.FullName, launcherScript.Name))
    if not (Directory.Exists(finalLauncherScriptInPrefix.Directory.FullName)) then
        Directory.CreateDirectory(finalLauncherScriptInPrefix.Directory.FullName) |> ignore
    File.Copy(launcherScript.FullName, finalLauncherScriptInPrefix.FullName, true)
    if Process.Execute({ Command = "chmod"; Arguments = sprintf "ugo+x %s" finalLauncherScriptInPrefix.FullName },
                       Echo.Off).ExitCode <> 0 then
        failwith "Unexpected chmod failure, please report this bug"

| Some("run") ->
    let oldVersionOfMono =
        let versionOfMonoWhereRunningExesDirectlyIsSupported = "5.16"

        match Misc.GuessPlatform() with
        | Misc.Platform.Windows ->
            // not using Mono anyway
            false
        | Misc.Platform.Mac ->
            // unlikely that anyone uses old Mono versions in Mac, as it's easy to update (TODO: detect anyway)
            false
        | Misc.Platform.Linux ->
            let pkgConfig = "pkg-config"
            if not (Process.CommandWorksInShell pkgConfig) then
                failwithf "'%s' was uninstalled after ./configure.sh was invoked?" pkgConfig
            let pkgConfigCmd = { Command = pkgConfig
                                 Arguments = sprintf "--atleast-version=%s mono"
                                                 versionOfMonoWhereRunningExesDirectlyIsSupported }
            let processResult = Process.Execute(pkgConfigCmd, Echo.OutputOnly)
            processResult.ExitCode <> 0

    let debug = BinaryConfig.Debug
    let frontend,_ = JustBuild debug

    let frontendDir,frontendExecutable = GetPathToFrontend frontend debug

    let pathToFrontend = frontendExecutable.FullName
    let startInfo =
        if oldVersionOfMono then
            ProcessStartInfo(FileName = "mono", Arguments = pathToFrontend, UseShellExecute = false)
        else
            ProcessStartInfo(FileName = pathToFrontend, UseShellExecute = false)
    startInfo.EnvironmentVariables.["MONO_ENV_OPTIONS"] <- "--debug"

    let proc = Process.Start startInfo
    proc.WaitForExit()

| Some "update-servers" ->
    let utxoCoinRelativePath = Path.Combine("src", "GWallet.Backend", "UtxoCoin")

    let btcServersUrl = "https://raw.githubusercontent.com/spesmilo/electrum/master/electrum/servers.json"
    let btcServersFileRelativePath = Path.Combine(utxoCoinRelativePath, "btc-servers.json")
    let updateBtc = Process.Execute ({ Command = "curl"
                                       Arguments = sprintf "--fail -o %s %s" btcServersFileRelativePath btcServersUrl },
                                       Echo.All)
    if (updateBtc.ExitCode <> 0) then
        Console.Error.WriteLine "Update failed"
        Environment.Exit 1

    let ltcServersUrl = "https://raw.githubusercontent.com/pooler/electrum-ltc/master/electrum_ltc/servers.json"
    let ltcServersFileRelativePath = Path.Combine(utxoCoinRelativePath, "ltc-servers.json")
    let updateLtc = Process.Execute ({ Command = "curl"
                                       Arguments = sprintf "--fail -o %s %s" ltcServersFileRelativePath ltcServersUrl },
                                       Echo.All)
    if (updateLtc.ExitCode <> 0) then
        Console.Error.WriteLine "Update failed"
        Environment.Exit 1

| Some(someOtherTarget) ->
    Console.Error.WriteLine("Unrecognized target: " + someOtherTarget)
    Environment.Exit 2
