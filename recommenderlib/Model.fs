namespace recommender
type Tag(name: string, weight: double) = 
    member this.Name with get() = name
    member this.Weight with get() = weight
type Resource(uri: string, tags: Tag list) =
    member this.Uri with get() = uri
    member this.Tags with get() = tags
    member this.AddTag (tag: Tag) =
        new Resource(uri, tag::tags)