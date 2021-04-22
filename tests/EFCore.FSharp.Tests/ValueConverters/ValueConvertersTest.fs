module EntityFrameworkCore.FSharp.Test.ValueConverters.ValueConvertersTest

open System
open Microsoft.EntityFrameworkCore.Storage.ValueConversion
open EntityFrameworkCore.FSharp
open Expecto

[<Tests>]
let ValueConvertersTest =
    testList "ValueConvertersTest" [

        test "StringOptionConverter" {
            let converter = FSharpValueConverterSelector(ValueConverterSelectorDependencies())

            let result = converter.Select(typeof<string option>, typeof<string>)

            Expect.equal (result |> Seq.length) 1 "Should only have one entry"

            let f = result |> Seq.head

            let typeConverter = f.Create()
            Expect.equal (typeConverter.GetType()) typeof<StringOptionConverter> "Should be equal"

        }

        test "GuidOptionConverter" {
            let converter = FSharpValueConverterSelector(ValueConverterSelectorDependencies())

            let result = converter.Select(typeof<Guid option>, typeof<Guid>)

            Expect.equal (result |> Seq.length) 1 "Should only have one entry"

            let f = result |> Seq.head

            let typeConverter = f.Create()
            Expect.equal (typeConverter.GetType()) typeof<StructOptionConverter<Guid>> "Should be equal"
            Expect.equal typeConverter.ModelClrType typeof<Guid option> "Should be equal"
            Expect.equal typeConverter.ProviderClrType typeof<Nullable<Guid>> "Should be equal"

        }

        //test "string -> string option" {
        //    let c = Conversion.toOption<string>
        //    Expect.equal (c.Compile().Invoke(null)) None "Should be equal"

        //    let g = "test"
        //    Expect.equal (c.Compile().Invoke(g)) (Some g) "Should be equal"
        //}

        //test "string option -> string" {
        //    let c = Conversion.fromOption<string>
        //    Expect.equal (c.Compile().Invoke(None)) null "Should be equal"

        //    let g = "test"
        //    Expect.equal (c.Compile().Invoke(Some g)) g "Should be equal"
        //}

        //test "Guid? -> Guid option" {
        //    let c = Conversion.toOption<Nullable<Guid>>
        //    Expect.equal (c.Compile().Invoke(Nullable())) None "Should be equal"

        //    let g = Nullable(Guid.NewGuid())
        //    Expect.equal (c.Compile().Invoke(g)) (Some g) "Should be equal"
        //}

        //test "Guid option -> Guid?" {
        //    let c = Conversion.fromOption<Nullable<Guid>>
        //    Expect.equal (c.Compile().Invoke(None)) (Nullable()) "Should be equal"

        //    let g = Nullable(Guid.NewGuid())
        //    Expect.equal (c.Compile().Invoke(Some g)) g "Should be equal"
        //}

        //test "Can create OptionConverter" {
        //    let oc = OptionConverter<string>()

        //    Expect.isNotNull (box oc) "Should not be null"
        //}
    ]
