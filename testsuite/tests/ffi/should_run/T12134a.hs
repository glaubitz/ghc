import Foreign.C.Types

foreign import ccall "many_floats" many :: CFloat -> CFloat ->
     CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat ->
     CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat ->
     CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat ->
     CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat ->
     CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat ->
     CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat ->
     CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat ->
     CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat ->
     CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat ->
     CFloat -> CFloat -> CFloat -> CDouble -> IO ()

main = many 1.5 2.5 3.5 4.5 5.5 6.5 7.5 8.5 9.5 10.5 11.5 12.5 13.5 14.5 15.5 16.5 17.5 18.5 19.5 20.5 21.5 22.5 23.5 24.5 25.5 26.5 27.5 28.5 29.5 30.5 31.5 32.5 33.5 34.5 35.5 36.5 37.5 38.5 39.5 40.5 41.5 42.5 43.5 44.5 45.5 46.5 47.5 48.5 49.5 50.5 51.5 52.5 53.5 54.5 55.5 56.5 57.5 58.5 59.5 60.5
