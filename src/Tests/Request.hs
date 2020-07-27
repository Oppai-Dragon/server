module Tests.Request where

import Network.Wai
import Network.Wai.Internal

testPostReq = defaultRequest
    { requestMethod = "POST"
    , queryString =
        [("first_name",Just "misha")
        ,("last_name",Just "dragon")
        ]
    , pathInfo = ["person","create"]
    }