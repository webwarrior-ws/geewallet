namespace GWallet.Backend.UtxoCoin

open GWallet.Backend

type ImportedAccount(_mnemonic: string) =    
    interface IAccount with
        member self.Currency = Currency.BTC
        member self.PublicAddress = "publicAddress"
 
    member self.GetTotalBalance(): Async<Option<decimal>> =
        async { return None }

    member self.SendFunds (_destinationAccount: IAccount) (_amount: TransferAmount): Async<string> =
        async { return "" }