module Types exposing
    ( Depiction(..)
    , EdgeId
    , Flag
    , ForTask(..)
    , Group(..)
    , GroupId
    , Like
    , Mention
    , MentionSuggestion
    , PageId
    , PageIdOrRemarkId
    , ParentId
    , PointIndex
    , Profile
    , Remark
    , RemarkId
    , Remarks(..)
    , RemarksData
    , RequestDelete
    , Role(..)
    , RscDepiction
    , User(..)
    , UserId
    , remarksToList
    )

import File exposing (File)
import Time exposing (Posix)


type alias PointIndex =
    Int


type alias PageIdOrRemarkId =
    Int


type alias EdgeId =
    Int


type alias ParentId =
    Int


type alias PageId =
    Int


type alias RemarkId =
    Int


type alias UserId =
    Int


type Group
    = DefaultGroup
    | Group GroupId


type alias GroupId =
    Int


type alias RequestDelete =
    Bool


type Role
    = CommunityLibrarian
    | Manager
    | ProjectLeader
    | Specialist Int
    | Member


type User
    = User Profile
    | Anonymous


type alias RemarksData =
    { remarks : List Remark
    , user : User
    , group : Group
    , ongoing_task : Bool
    }


type Remarks
    = Remarks (List Remark)


type alias Profile =
    { userId : UserId
    , userName : String
    , userSubtitle : String
    , userAvatarUrl : String
    , userUrl : String
    , roles : List Role
    }


type alias Remark =
    { id : RemarkId
    , isPublished : Bool
    , body : String
    , date : Posix
    , timezone : Time.Zone
    , likes : List Like
    , flags : List Flag
    , replies : Remarks
    , mentions : List Mention
    , depiction : Depiction
    , for_task : Bool
    , author : Profile
    }


type alias Like =
    { edgeId : EdgeId
    , userId : Int
    , userName : String
    }


type alias Flag =
    { edgeId : EdgeId
    , userId : Int
    , userName : String
    }


type alias Mention =
    { userId : Int
    , userName : String
    , userUrl : String
    }


type alias MentionSuggestion =
    { userId : UserId
    , userName : String
    }


type Depiction
    = NewDepiction File
    | ExistingDepiction RscDepiction
    | NoDepiction


type alias RscDepiction =
    { depictionId : Int
    , depictionTitle : String
    , depictionUrl : String
    }


type ForTask
    = NoOngoingTask
    | IsTaskSubmission
    | IsNotTaskSubmission



-- HELPER FUNCTIONS


remarksToList : Remarks -> List Remark
remarksToList (Remarks remarks) =
    remarks
