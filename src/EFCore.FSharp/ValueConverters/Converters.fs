namespace EntityFrameworkCore.FSharp

open System
open System.Collections.Concurrent
open Microsoft.EntityFrameworkCore.Storage.ValueConversion
open EntityFrameworkCore.FSharp.Internal.FSharpUtilities

module internal ConverterImpl =

    let createFromOption<'a when 'a :> ValueType and 'a : struct and 'a : (new: unit -> 'a)>() =
        (fun (x : 'a option) -> match x with | Some y -> Nullable(y) | None -> Nullable()) |> convertFunToExpr

    let createToOption<'a when 'a :> ValueType and 'a : struct and 'a : (new: unit -> 'a)>() =
        (fun (x: Nullable<'a>) -> if x.HasValue then Some x.Value else None) |> convertFunToExpr

type StringOptionConverter (converterMappingHints) =
    inherit ValueConverter<string option, string>(StringOptionConverter.FromOption, StringOptionConverter.ToOption, converterMappingHints)

    static member internal FromOption =
        (fun (x : string option) -> match x with Some y -> y | None -> null) |> convertFunToExpr

    static member internal ToOption =
        (fun (x: string) -> if isNull x then None else Some x) |> convertFunToExpr

type ByteArrayOptionConverter (converterMappingHints) =
    inherit ValueConverter<byte[] option, byte[]>(ByteArrayOptionConverter.FromOption, ByteArrayOptionConverter.ToOption, converterMappingHints)

    static member internal FromOption =
        (fun (x : byte[] option) -> match x with Some y -> y | None -> null) |> convertFunToExpr

    static member internal ToOption =
        (fun (x: byte[]) -> if isNull x then None else Some x) |> convertFunToExpr

type StructOptionConverter<'a when 'a :> ValueType and 'a : struct and 'a : (new: unit -> 'a)>
    (converterMappingHints) =
    inherit ValueConverter<'a option, Nullable<'a>>(StructOptionConverter<'a>.FromOption, StructOptionConverter<'a>.ToOption, converterMappingHints)

    static member internal FromOption =
        (fun (x : 'a option) -> match x with | Some y -> Nullable(y) | None -> Nullable()) |> convertFunToExpr

    static member internal ToOption =
        (fun (x: Nullable<'a>) -> if x.HasValue then Some x.Value else None) |> convertFunToExpr

type StringOptionToStringConverter(mappingHints) =
    inherit StringOptionConverter(mappingHints)

    static member DefaultInfo =
        new ValueConverterInfo(
            typeof<string option>,
            typeof<string>,
            (fun i -> (StringOptionConverter i.MappingHints :> ValueConverter)),
            null
        )

type ByteArrayOptionToByteArrayConverter(mappingHints) =
    inherit ByteArrayOptionConverter(mappingHints)

    static member DefaultInfo =
        new ValueConverterInfo(
            typeof<byte[] option>,
            typeof<byte[]>,
            (fun i -> (ByteArrayOptionConverter i.MappingHints :> ValueConverter)),
            null
        )

type StructOptionToStructConverter<'a when 'a :> ValueType and 'a : struct and 'a : (new: unit -> 'a)>(mappingHints) =
    inherit StructOptionConverter<'a>(mappingHints)

    static member DefaultInfo =
        new ValueConverterInfo(
            typeof<'a option>,
            typeof<'a>,
            (fun i -> (StructOptionConverter<'a> i.MappingHints :> ValueConverter)),
            null
        )

type FSharpValueConverterSelector(dependencies) =
    inherit ValueConverterSelector(dependencies)

    let _converters = ConcurrentDictionary<(Type * Type), ValueConverterInfo>()

    member private __.CreateStructConverter(modelClrType : Type, underlyingModelType : Type, underlyingProviderType : Type) =
        let converterType = typedefof<StructOptionConverter<_>>.MakeGenericType(underlyingProviderType)
        
        if not (isNull converterType) then
            _converters.GetOrAdd(
                (underlyingModelType, underlyingProviderType),
                fun k ->
                    let factory = (fun (info:ValueConverterInfo) -> Activator.CreateInstance(converterType, info.MappingHints) :?> ValueConverter)
                    ValueConverterInfo(modelClrType, underlyingProviderType, Func<ValueConverterInfo, ValueConverter>(factory))
                ) |> Some
        else
            None

    override this.Select(modelClrType, providerClrType) =

        // Call to base must be outisde of the seq computation expression
        let baseConverters = base.Select(modelClrType, providerClrType)

        seq {
            yield! baseConverters

            let underlyingModelType = if isNull modelClrType then null else SharedTypeExtensions.unwrapNullableType modelClrType
            let underlyingProviderType = if isNull providerClrType then null else SharedTypeExtensions.unwrapNullableType providerClrType

            if isNull underlyingProviderType || underlyingProviderType = typeof<string> then
                let converterType = typedefof<StringOptionConverter>

                if not (isNull converterType) then
                    yield _converters.GetOrAdd(
                        (underlyingModelType, underlyingProviderType),
                        fun k ->
                            let factory = (fun (info:ValueConverterInfo) -> Activator.CreateInstance(converterType, info.MappingHints) :?> ValueConverter)
                            ValueConverterInfo(modelClrType, typeof<string>, Func<ValueConverterInfo, ValueConverter>(factory))
                        )

            if isNull underlyingProviderType || underlyingProviderType = typeof<byte[]> then
                let converterType = typedefof<ByteArrayOptionConverter>

                if not (isNull converterType) then
                    yield _converters.GetOrAdd(
                        (underlyingModelType, underlyingProviderType),
                        fun k ->
                            let factory = (fun (info:ValueConverterInfo) -> Activator.CreateInstance(converterType, info.MappingHints) :?> ValueConverter)
                            ValueConverterInfo(modelClrType, typeof<byte[]>, Func<ValueConverterInfo, ValueConverter>(factory))
                        )

            if isNull underlyingProviderType || underlyingProviderType = typeof<bool> then
                let createConverter = this.CreateStructConverter(modelClrType, underlyingModelType, typeof<bool>)
                if createConverter.IsSome then
                    yield createConverter.Value
            
            if isNull underlyingProviderType || underlyingProviderType = typeof<byte> then
                let createConverter = this.CreateStructConverter(modelClrType, underlyingModelType, typeof<byte>)
                if createConverter.IsSome then
                    yield createConverter.Value
            
            if isNull underlyingProviderType || underlyingProviderType = typeof<char> then
                let createConverter = this.CreateStructConverter(modelClrType, underlyingModelType, typeof<char>)
                if createConverter.IsSome then
                    yield createConverter.Value
            
            if isNull underlyingProviderType || underlyingProviderType = typeof<DateTime> then
                let createConverter = this.CreateStructConverter(modelClrType, underlyingModelType, typeof<DateTime>)
                if createConverter.IsSome then
                    yield createConverter.Value
            
            if isNull underlyingProviderType || underlyingProviderType = typeof<DateTimeOffset> then
                let createConverter = this.CreateStructConverter(modelClrType, underlyingModelType, typeof<DateTimeOffset>)
                if createConverter.IsSome then
                    yield createConverter.Value
            
            if isNull underlyingProviderType || underlyingProviderType = typeof<decimal> then
                let createConverter = this.CreateStructConverter(modelClrType, underlyingModelType, typeof<decimal>)
                if createConverter.IsSome then
                    yield createConverter.Value
            
            if isNull underlyingProviderType || underlyingProviderType = typeof<double> then
                let createConverter = this.CreateStructConverter(modelClrType, underlyingModelType, typeof<double>)
                if createConverter.IsSome then
                    yield createConverter.Value
            
            if isNull underlyingProviderType || underlyingProviderType = typeof<Guid> then
                let createConverter = this.CreateStructConverter(modelClrType, underlyingModelType, typeof<Guid>)
                if createConverter.IsSome then
                    yield createConverter.Value
            
            if isNull underlyingProviderType || underlyingProviderType = typeof<int16> then
                let createConverter = this.CreateStructConverter(modelClrType, underlyingModelType, typeof<int16>)
                if createConverter.IsSome then
                    yield createConverter.Value
            
            if isNull underlyingProviderType || underlyingProviderType = typeof<int> then
                let createConverter = this.CreateStructConverter(modelClrType, underlyingModelType, typeof<int>)
                if createConverter.IsSome then
                    yield createConverter.Value
            
            if isNull underlyingProviderType || underlyingProviderType = typeof<int64> then
                let createConverter = this.CreateStructConverter(modelClrType, underlyingModelType, typeof<int64>)
                if createConverter.IsSome then
                    yield createConverter.Value
            
            if isNull underlyingProviderType || underlyingProviderType = typeof<sbyte> then
                let createConverter = this.CreateStructConverter(modelClrType, underlyingModelType, typeof<sbyte>)
                if createConverter.IsSome then
                    yield createConverter.Value
            
            if isNull underlyingProviderType || underlyingProviderType = typeof<float> then
                let createConverter = this.CreateStructConverter(modelClrType, underlyingModelType, typeof<float>)
                if createConverter.IsSome then
                    yield createConverter.Value
            
            if isNull underlyingProviderType || underlyingProviderType = typeof<TimeSpan> then
                let createConverter = this.CreateStructConverter(modelClrType, underlyingModelType, typeof<TimeSpan>)
                if createConverter.IsSome then
                    yield createConverter.Value
            
            if isNull underlyingProviderType || underlyingProviderType = typeof<uint16> then
                let createConverter = this.CreateStructConverter(modelClrType, underlyingModelType, typeof<uint16>)
                if createConverter.IsSome then
                    yield createConverter.Value
            
            if isNull underlyingProviderType || underlyingProviderType = typeof<uint> then
                let createConverter = this.CreateStructConverter(modelClrType, underlyingModelType, typeof<uint>)
                if createConverter.IsSome then
                    yield createConverter.Value
            
            if isNull underlyingProviderType || underlyingProviderType = typeof<uint64> then
                let createConverter = this.CreateStructConverter(modelClrType, underlyingModelType, typeof<uint64>)
                if createConverter.IsSome then
                    yield createConverter.Value
            
        }


    
module Converters =

    let stringOptionConverter =
        ValueConverter<string option, string>(
            (fun x -> match x with Some v -> v | None -> null),
            (fun x -> if x = null then None else Some x)
        )

    let structOptionConverter<'a when 'a :> ValueType and 'a : (new: unit -> 'a) and 'a : struct>() =
        ValueConverter<'a option, Nullable<'a>>(
            (fun x -> match x with Some v -> Nullable(v) | None -> Nullable()),
            (fun x -> if x.HasValue then Some x.Value else None)
        )
