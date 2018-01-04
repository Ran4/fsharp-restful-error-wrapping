#load "Rop.fs"
open Rop

//Debug variables, toggle to test out different outcomes
let FAIL_GETTING_BANKACCOUNTID_FROM_RESPONSE = false
let FAIL_GETTING_BANK_ACCOUNT_FROM_PROVIDER = false


module CoreTypes =
    type HttpResponse =
        | HttpResponse of statusCode: int * body: string
    with
        static member mk statusCode body =
            HttpResponse (statusCode, body)

    type Error =
        | BadRequestError of body: string option
        | InternalError of body: string option
    with
        static member toHttpResponse =
            function
            | BadRequestError (Some body) -> HttpResponse (statusCode=400, body=body)
            | BadRequestError None        -> HttpResponse (statusCode=400, body="")
            | InternalError (Some body)   -> HttpResponse (statusCode=500, body=body)
            | InternalError None          -> HttpResponse (statusCode=500, body="")

module Entities =
    type BankAccount = {
        id: string
    } with
        static member toJson this =
            sprintf """{"id": %s}""" this.id

    type BankIdNumber =
        string
        
    
module Provider =
    open CoreTypes
    open Entities
    let getBankAccount (bankIdNumber: BankIdNumber): Result<BankAccount, Error> =
        if FAIL_GETTING_BANK_ACCOUNT_FROM_PROVIDER
        then Failure (InternalError (Some "Provider failure"))
        else Success { id = "ab-de-gh" }

            
module ViewHelpers =
    open CoreTypes
    let throughHttpContext f =
        either f Error.toHttpResponse
    
    
module ExampleViews =
    open CoreTypes
    open Entities
    open ViewHelpers
    
    let internal getBankAccountIdFromIncomingResponse r =
        if FAIL_GETTING_BANKACCOUNTID_FROM_RESPONSE
        then Failure (BadRequestError (Some "Couldn't find a BankAccountId"))
        else Success ("hello": BankIdNumber)
        
    // Fairly explicit, using either:
    let view1 r =
        getBankAccountIdFromIncomingResponse r
        >>= Provider.getBankAccount
        |> either (BankAccount.toJson >> HttpResponse.mk 200) Error.toHttpResponse
        
    // Since we'll probably do this often:
    let view2 r =
        getBankAccountIdFromIncomingResponse r
        >>= Provider.getBankAccount
        |> throughHttpContext (BankAccount.toJson >> HttpResponse.mk 200)
        
    // More explicit using more functions:
    let view3 r =
        let mkBankAccountResponse bankAccount =
            let body = bankAccount |> BankAccount.toJson
            HttpResponse (statusCode=200, body=body)
            
        getBankAccountIdFromIncomingResponse r
        >>= Provider.getBankAccount
        |> throughHttpContext mkBankAccountResponse
        
    // This version retains the Success/Fail context from getBankAccount
    let view4 r =
        getBankAccountIdFromIncomingResponse r
        >>= Provider.getBankAccount
        >>! fail Error.toHttpResponse
        >>= switch (BankAccount.toJson >> HttpResponse.mk 200)
    
    let r = "whatever"
    printfn "view1: %A" <| view1 r
    printfn "view2: %A" <| view2 r
    printfn "view3: %A" <| view3 r
    printfn "view4: %A" <| view3 r
