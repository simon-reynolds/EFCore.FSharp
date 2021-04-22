namespace EntityFrameworkCore.FSharp

open System
open Microsoft.EntityFrameworkCore
open Microsoft.EntityFrameworkCore.Metadata
open Microsoft.EntityFrameworkCore.Storage.ValueConversion

module Extensions =

    //let private genericOptionConverterType = typedefof<OptionConverter<_>>

    let private fixRequiredAttributes (mb: ModelBuilder) =
    
        let isPropertyRequired (p: IProperty) =
            (p.IsPrimaryKey()) ||
            not (SharedTypeExtensions.isOptionType p.ClrType || SharedTypeExtensions.isNullableType p.ClrType)
    
        mb.Model.GetEntityTypes()
        |> Seq.iter(fun e ->
            e.GetProperties()
            |> Seq.iter(fun p ->
                let isRequired = isPropertyRequired p
                mb.Entity(e.Name).Property(p.Name).IsRequired(isRequired) |> ignore

            )
        )

    type ModelBuilder with

        member this.FixRequiredAttributes() =
            fixRequiredAttributes this

        member this.UseValueConverterForType<'a>(converter : ValueConverter) =
            this.UseValueConverterForType(typeof<'a>, converter)

        member this.UseValueConverterForType(``type`` : Type, converter : ValueConverter) =

            fixRequiredAttributes this

            this.Model.GetEntityTypes()
            |> Seq.iter(fun e ->
                e.GetProperties()
                |> Seq.filter(fun p -> p.ClrType = ``type``)
                |> Seq.iter(fun p ->
                    this.Entity(e.Name).Property(p.Name).HasConversion(converter) |> ignore
                )
            )

            this

        //member this.RegisterOptionTypes() =

        //    fixRequiredAttributes this

        //    let makeOptionConverter t =
        //        let underlyingType = SharedTypeExtensions.unwrapOptionType t
        //        let converterType = genericOptionConverterType.MakeGenericType(underlyingType)
        //        let converter = converterType.GetConstructor([||]).Invoke([||]) :?> ValueConverter
        //        converter
            
        //    let converterDetails =
        //        this.Model.GetEntityTypes()
        //        |> Seq.collect (fun e -> e.GetProperties())
        //        |> Seq.filter (fun p -> SharedTypeExtensions.isOptionType p.ClrType && p.IsNullable)
        //        |> Seq.map(fun p -> (p.Name, p.DeclaringType.Name, (makeOptionConverter p.ClrType)) )
                
        //    converterDetails
        //    |> Seq.iter(fun (propName, entityName, converter) ->
        //        this.Entity(entityName).Property(propName).HasConversion(converter) |> ignore
        //    )

    //let registerOptionTypes (modelBuilder : ModelBuilder) =
    //    modelBuilder.RegisterOptionTypes()

    let useValueConverter<'a> (converter : ValueConverter) (modelBuilder : ModelBuilder) =
        modelBuilder.UseValueConverterForType<'a>(converter)

    let useValueConverterForType (``type`` : Type) (converter : ValueConverter) (modelBuilder : ModelBuilder) =
        modelBuilder.UseValueConverterForType(``type``, converter)            
