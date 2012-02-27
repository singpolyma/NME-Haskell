{-# LANGUAGE ForeignFunctionInterface #-}

module Text.NME where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS (toString, fromString)

-- | Takes a string and options, returns Either an error code or a string
--   in the specified format
process :: String -> [ProcessOpt] -> String -> Ptr OutputFormat -> Int -> Either Err String
process [] _ _ _ _ = Right []
process input options eol outputFormat fontSize = unsafeLocalState $
	io_process input options eol outputFormat fontSize

newtype OutputFormat = OutputFormat (Ptr OutputFormat)

foreign import ccall unsafe "NME.h &NMEOutputFormatText"
	outputFormatText ::
	Ptr OutputFormat

foreign import ccall unsafe "NME.h &NMEOutputFormatTextCompact"
	outputFormatTextCompact ::
	Ptr OutputFormat

foreign import ccall unsafe "NME.h &NMEOutputFormatDebug"
	outputFormatDebug ::
	Ptr OutputFormat

foreign import ccall unsafe "NME.h &NMEOutputFormatNull"
	outputFormatNull ::
	Ptr OutputFormat

foreign import ccall unsafe "NME.h &NMEOutputFormatNME"
	outputFormatNME ::
	Ptr OutputFormat

foreign import ccall unsafe "NME.h &NMEOutputFormatHTML"
	outputFormatHTML ::
	Ptr OutputFormat

foreign import ccall unsafe "NME.h &NMEOutputFormatRTF"
	outputFormatRTF ::
	Ptr OutputFormat

foreign import ccall unsafe "NME.h &NMEOutputFormatLaTeX"
	outputFormatLaTeX ::
	Ptr OutputFormat

foreign import ccall unsafe "NME.h &NMEOutputFormatMan"
	outputFormatMan ::
	Ptr OutputFormat

foreign import ccall unsafe "NME.h NMEProcess"
	c_nmeProcess ::
	CString -> CInt -> CString -> CInt -> CInt -> CString ->
	Ptr OutputFormat -> CInt -> Ptr CString -> Ptr CInt -> Ptr CInt ->
	IO Err

io_process :: String -> [ProcessOpt] -> String -> Ptr OutputFormat -> Int -> IO (Either Err String)
io_process input options eol outputFormat fontSize =
	BS.useAsCStringLen (BS.fromString input) (\ (c_input, c_input_len) ->
		BS.useAsCString (BS.fromString eol) (\ c_eol ->
			alloca (\ c_output ->
				alloca (\ c_output_len -> do
					let try_nmeProcess buf bufLen = do
						err <- c_nmeProcess c_input (fromIntegral c_input_len)
						       buf (fromIntegral bufLen) c_options c_eol
						       outputFormat (fromIntegral fontSize) c_output
						       c_output_len nullPtr
						case err of
							Err 1 -> do
								buf <- reallocBytes buf (bufLen * 2)
								try_nmeProcess buf (bufLen * 2);
							Err 0 -> do
								output <- peek c_output
								bs_output <- BS.packCString output
								free buf
								return . Right $ BS.toString bs_output
							_ -> do
								free buf
								return . Left $ err

					let bufLen = c_input_len * 2
					buf <- mallocBytes bufLen
					try_nmeProcess buf bufLen
				)
			)
		)
	)
	where
	c_options = foldr (\(ProcessOpt o) acc -> acc .|. o) 0 options

-- WARNING: If the NME enum definitions change, then these have to change

newtype Err = Err CInt deriving (Show, Eq)
#enum Err, Err, \
	kNMEErrOk = 0, \
	kNMEErrNotEnoughMemory = (0 + 1), \
	kNMEErrBadMarkup = ((0 + 1) + 1), \
	kNMEErrInternal = (((0 + 1) + 1) + 1), \
	kNMEErr1stNMEOpt = ((((0 + 1) + 1) + 1) + 1), \
	kNMEErr1stUser = 10000

newtype ProcessOpt = ProcessOpt CInt deriving (Show, Eq)
#enum ProcessOpt, ProcessOpt, \
	kNMEProcessOptDefault = 0, \
	kNMEProcessOptNoPreAndPost = 0x1, \
	kNMEProcessOptNoH1 = 0x4, \
	kNMEProcessOptH1Num = 0x8, \
	kNMEProcessOptH2Num = 0x10, \
	kNMEProcessOptNoDL = 0x20, \
	kNMEProcessOptNoIndentedPar = 0x40, \
	kNMEProcessOptNoMultilinePar = 0x80, \
	kNMEProcessOptNoEscape = 0x100, \
	kNMEProcessOptNoHRule = 0x200, \
	kNMEProcessOptNoLink = 0x400, \
	kNMEProcessOptNoImage = 0x800, \
	kNMEProcessOptNoTable = 0x1000, \
	kNMEProcessOptNoUnderline = 0x2000, \
	kNMEProcessOptNoMonospace = 0x4000, \
	kNMEProcessOptNoSubSuperscript = 0x8000, \
	kNMEProcessOptNoBold = 0x10000, \
	kNMEProcessOptNoItalic = 0x20000, \
	kNMEProcessOptNoPlugin = 0x40000, \
	kNMEProcessOptVerbatimMono = 0x100000, \
	kNMEProcessOptXRef = 0x200000
