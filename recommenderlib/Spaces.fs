namespace recommender

type public RecommendationResult(uri: string, recommendations: (double * string) seq) =
    member this.Uri with get() = uri
    member this.Result with get() = recommendations
    member this.Recommendations 
        with get() =
            recommendations
            |> Seq.map (fun tup -> snd(tup))
type public VectorSpace(resources: Resource seq) = 
    let square x = x * x
    let normalize (r: Resource) =
        let norm : double =
            r.Tags
            |> List.fold (fun acc t -> acc + (square t.Weight)) 0.0
            |> System.Math.Sqrt
        r.Tags
        |> List.map (fun i -> (i.Name, i.Weight / norm))
        |> List.sortBy (fun i -> fst(i))
    let distance (r1: (string * double) list) (r2: (string * double) list) =
        let rec combinedTuples l1 l2: (string * double * double) seq = seq {
                match r1 with
                | [] ->
                    yield! l2 |> List.map (fun i -> (fst(i), 0.0, snd(i)))
                | h1::t1 ->
                    let name1 = fst(h1)
                    match l2 with
                    | [] ->
                        yield! l1 |> List.map (fun i -> (name1, snd(i), 0.0))
                    | h2::t2 ->
                        let name2 = fst(h1)
                        match System.String.Compare(name1, name2) with
                        | 0 ->
                            yield (name1, snd(h1), snd(h2))
                            yield! combinedTuples t1 t2
                        | cmp when cmp < 0 ->
                            yield (name1, snd(h1), 0.0)
                            yield! combinedTuples t1 l2
                        | _ ->
                            yield (name2, 0.0, snd(h2))
                            yield! combinedTuples l1 t2
            }
        combinedTuples r1 r2
        |> Seq.map (fun (name, n1, n2) -> 
                        square(n1 - n2)
                   )
        |> Seq.sum
        |> System.Math.Sqrt        
        
    member this.ComputeRecommendations (elementCount: int): RecommendationResult seq =
        ///Returns the list of resources that at least share a tag with r
        let getSharingResources (r: Resource): Resource seq =
            resources
            |> Seq.filter (
                            fun otherResource ->
                                (otherResource.Uri <> r.Uri) &&
                                (
                                    otherResource.Tags
                                    |> Seq.exists (
                                                    fun otherTag ->
                                                        r.Tags
                                                        |> Seq.exists (fun resourceTag -> resourceTag.Name = otherTag.Name)
                                                  )
                                )
                           )
        
        let getSubUniverse (r: Resource) =
            let sharingResources =
                r
                |> getSharingResources
            let tagList =
                sharingResources
                |> Seq.map (
                                fun otherResource ->
                                    otherResource.Tags
                                    |> Seq.map (fun otherTag -> otherTag.Name)
                           )
                |> Seq.fold (fun acc s -> Seq.append acc s) Seq.empty
                |> Seq.append (r.Tags |> Seq.map (fun myTag -> myTag.Name))
                |> Seq.distinct
            (sharingResources, tagList)
        let getRecommendation (sharingResources: Resource seq) (tagNames: string list) (r: Resource): RecommendationResult =
            let relativeDistance = 
                r
                |> normalize
                |> distance
            let processResource = (normalize >> relativeDistance)
            let result =
                sharingResources
                |> Seq.map (fun other -> (other |> processResource), other.Uri)
                |> Seq.take elementCount
            new RecommendationResult(r.Uri, result)
        resources
        |> Seq.map (fun r -> (r, getSubUniverse r))
        |> Seq.map (fun tuple ->
                        match tuple with
                        | (res, (sharingRes, tagList)) -> getRecommendation sharingRes (tagList |> List.ofSeq) res
                   )