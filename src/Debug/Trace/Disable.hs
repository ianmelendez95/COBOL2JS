module Debug.Trace.Disable where 

trace :: String -> a -> a
trace _ x = x