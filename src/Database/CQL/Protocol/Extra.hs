    
module Database.CQL.Protocol.Extra where
      
import Database.CQL.Protocol
  ( QueryParams(..)  
  , Consistency (One, LocalQuorum)
  )
  
defQueryParams :: a -> QueryParams a
defQueryParams a = QueryParams One True a Nothing Nothing Nothing

defQueryParamsMeta :: a -> QueryParams a               
defQueryParamsMeta a = QueryParams One False a Nothing Nothing Nothing       
