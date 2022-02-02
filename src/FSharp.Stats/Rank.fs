namespace FSharp.Stats


/// Module to Calculate the rank. The rank of a number is its size relative to other values in a sequence
module Rank =

    /// Comparer that sorts nan at the end of a collection
    let inline private compNaNLast<'T when 'T :> System.IComparable> =
        let comparison = 
            System.Comparison(fun (a : 'T when 'T :> System.IComparable) b -> 
                if nan.Equals(a) then 
                    1 
                elif nan.Equals(b) then 
                    -1
                else 
                    System.Collections.Generic.Comparer.Default.Compare(a,b)
            )
        System.Collections.Generic.Comparer<'T>.Create(comparison)

    /// Comparer that sorts nan at the start of a collection
    let inline private compNaNFirst<'T when 'T :> System.IComparable> = 
        System.Collections.Generic.Comparer.Default

    /// Ranks each entry of the given unsorted data array. Use 'breakTies function to break ties
    let inline private rank (breakTies: int -> int -> 'b) (convert: int -> 'b) (data:array<'a>) (comparer: System.Collections.Generic.Comparer<'b>) : array<'b> =
        let zero = LanguagePrimitives.GenericZero< 'b > //here wenn float, dann nan
        let data' = Array.copy data
        let ranks  = Array.create data.Length zero
        let index = Array.init data.Length id
        System.Array.Sort(data',index,comparer=comparer)

        let setTies a b =
            let tmp = breakTies a b
            for j = a to b-1 do
                ranks.[index.[j]] <- tmp

        let rec loop i pi =
            if i < data.Length then
                if nan.Equals(data'.[pi]) then          //new
                    //ranks.[index.[pi]] <- nan         //new
                    loop (i+1) i                        //new
                elif (abs (data'.[i] - data'.[pi]) = zero) then
                    loop (i+1) pi
                else
                    if (i = pi + 1) then
                        ranks.[index.[pi]] <- convert i
                    else
                        //break ties
                        setTies pi i

                    loop (i+1) (i)
            else
                //break ties if left over                
                //setTies pi i
                if (i = pi + 1) then
                    if nan.Equals(data'.[pi]) then     //new
                        //ranks.[index.[pi]] <- nan    //new
                        ()                             //new 
                    else ranks.[index.[pi]] <- convert i
                else
                    //break ties
                    setTies pi i

        loop 1 0 |> ignore
        ranks

    /// Ranks each entry of the given unsorted data array.
    /// Permutation with increasing values at each index of ties. nans are treated as individual smallest ranks.
    let inline rankFirst (data:array<_>) =
        let data' = Array.copy data
        let ranks  = Array.create data.Length 0.
        let index = Array.init data.Length id
        System.Array.Sort(data',index)
        for i=0 to ranks.Length-1 do
            ranks.[index.[i]] <- float (i + 1)
        ranks

    /// Ranks each entry of the given unsorted data array.
    /// Ties are replaced by their minimum. nans are treated as individual smallest ranks.
    let inline rankMin (data:array<_>) =    
        //let one = LanguagePrimitives.GenericOne< 'b > 
        let minTies a _ =  (float a + 1.)
        rank minTies float data compNaNFirst

    /// Ranks each entry of the given unsorted data array.
    /// Ties are replaced by their maximum. nans are treated as individual smallest ranks.
    let inline rankMax (data:array<_>) =    
        let maxTies _ b = float b
        rank maxTies float data compNaNFirst

    /// Ranks each entry of the given unsorted data array.
    /// Ties are replaced by their mean. nans are treated as individual smallest ranks.
    let inline rankAverage (data:array<_>) =    
        let averageTies a b = float (a + b + 1) / 2.//([(a + 1) .. b] |> List.sum) / float (b - a)
        rank averageTies float data compNaNFirst



    /// Ranks each entry of the given unsorted data array.
    /// Permutation with increasing values at each index of ties. nans are treated as individual highest ranks.
    let inline rankFirstNaNLast (data:array<_>) =
        //let ranks  = Array.copy data
        let data' = Array.copy data
        let ranks  = Array.create data.Length 0.
        let index = Array.init data.Length id
        System.Array.Sort(data',index,comparer=compNaNLast)
        for i=0 to ranks.Length-1 do
            ranks.[index.[i]] <- float (i + 1)
        ranks

    /// Ranks each entry of the given unsorted data array.
    /// Ties are replaced by their minimum. nans are treated as individual highest ranks.
    let inline rankMinNaNLast (data:array<_>) =    
        //let one = LanguagePrimitives.GenericOne< 'b > 
        let minTies a _ =  (float a + 1.)
        rank minTies float data compNaNLast

    /// Ranks each entry of the given unsorted data array.
    /// Ties are replaced by their maximum. nans are treated as individual highest ranks.
    let inline rankMaxNaNLast (data:array<_>) =    
        let maxTies _ b = float b
        rank maxTies float data compNaNLast

    /// Ranks each entry of the given unsorted data array.
    /// Ties are replaced by their mean. nans are treated as individual highest ranks.
    let inline rankAverageNaNLast (data:array<_>) =    
        let averageTies a b = float (a + b + 1) / 2.//([(a + 1) .. b] |> List.sum) / float (b - a)
        rank averageTies float data compNaNLast

