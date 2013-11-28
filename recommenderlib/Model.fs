namespace recommender
open Newtonsoft.Json

type public Tag() = 
    [<JsonProperty("name")>]    member val Name: string = null with get, set
    [<JsonProperty("weight")>]  member val Weight: double = 0.0 with get, set
type public Resource() =
    [<JsonProperty("uri")>]  member val Uri: string = null with get, set
    [<JsonConverter(typeof<ListConverter>)>]
    [<JsonProperty("tags")>] member val Tags: Tag list = [] with get, set
    member this.AddTag (tag: Tag) =
        new Resource(Uri=this.Uri, Tags=tag::this.Tags)