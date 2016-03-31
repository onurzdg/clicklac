
module Data.Composition where
      
infixr 9 .*
infixr 9 .**
infixr 9 .***       

(.*) :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)       
(.*) = fmap . fmap

(.**) :: (Functor f, Functor f1, Functor f2) 
      => (a -> b) -> f (f1 (f2 a)) -> f (f1 (f2 b))
(.**) = (.*) . fmap      

(.***) :: (Functor f, Functor f1, Functor f2, Functor f3)
       => (a -> b) -> f (f1 (f2 (f3 a))) -> f (f1 (f2 (f3 b)))
(.***) =  (.**) . fmap             
