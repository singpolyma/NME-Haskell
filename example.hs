import qualified Text.NME as NME

main :: IO ()
main = do
	input <- getContents
	case NME.process input [NME.kNMEProcessOptNoPreAndPost] "\n" NME.outputFormatHTML 0 of
		Left e -> print e
		Right s -> putStrLn s
