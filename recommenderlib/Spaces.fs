namespace recommender


type RecommendationResult(resource: Resource, recommendations: (decimal * Resource) list) =
    member this.Resource with get() = resource
    member this.Result with get() = recommendations
    member this.Recommendations 
        with get() =
            recommendations
            |> List.map (fun tup -> snd(tup))
type VectorSpace(resources: Resource list) = 
    let norm =
        let square x = x * x
        resources
        |> List.fold (
                        fun (acc: double) (r:Resource) ->
                            r.Tags
                            |> List.fold (fun acc t -> acc + t.Weight) 0.0
                            |> square
                            |> (+) acc
                     ) 0.0
        |> System.Math.Sqrt
    member this.ComputeRecommendations (tolerance: double): RecommendationResult option seq =
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
        let getRecommendation (sharingResources: Resource seq) (tagNames: string seq) (r: Resource): RecommendationResult option =
            None
//            resources
//            |> List.map (
//                            fun otherResource -> 
//                                otherResource.Tags
//                                |> List.map (fun tag -> tag.Weight / norm)
//                        )
        resources
        |> Seq.map (fun r -> (r, getSubUniverse r))
        |> Seq.map (fun tuple ->
                        match tuple with
                        | (res, (sharingRes, tagList)) -> getRecommendation sharingRes tagList res
                   )