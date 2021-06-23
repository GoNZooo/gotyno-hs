module ImportExample

open Thoth.Json.Net



type UsesImport =
    {
        ``type``: string
        recruiter: Basic.Recruiter
    }

    static member Decoder: Decoder<UsesImport> =
        Decode.object (fun get ->
            {
                ``type`` = get.Required.Field "type" (GotynoCoders.decodeLiteralString "UsesImport")
                recruiter = get.Required.Field "recruiter" Basic.Recruiter.Decoder
            }
        )

    static member Encoder value =
        Encode.object
            [
                "type", Encode.string "UsesImport"
                "recruiter", Basic.Recruiter.Encoder value.recruiter
            ]