namespace GWallet.Frontend.Maui

open System 
open Gtk 
open Microsoft.Maui 
open Microsoft.Maui.Graphics 
open Microsoft.Maui.Hosting 

type GtkApp() = 
    inherit MauiGtkApplication(Name = GWallet.Backend.Config.AppName)

    override _.CreateMauiApp() = MauiProgram.CreateMauiApp()
