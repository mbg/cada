--------------------------------------------------------------------------------
-- Cada Programming Language                                                  --
-- Copyright 2016 Michael B. Gale                                             --
--------------------------------------------------------------------------------

module Cada.Compiler.Internal.JS.ES6 (es6) where

--------------------------------------------------------------------------------

import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                          as T
import           Data.Text.Encoding                 (decodeUtf8)

import           Servant.Foreign
import           Servant.JS
import           Servant.JS.Internal

--------------------------------------------------------------------------------

-- | `es6 opts` generates ES6 bindings for an API.
es6 :: CommonGeneratorOptions -> JavaScriptGenerator
es6 opts = generateClass opts . mconcat . map (generateAPI opts)

--------------------------------------------------------------------------------

generateClass :: CommonGeneratorOptions -> T.Text -> T.Text
generateClass CommonGeneratorOptions{..} ms =
    "'use babel'\n\n" <>
    "import $ from 'jquery';\n\n" <>
    "export class " <> moduleName <> " {\n" <>
    "\tconstructor(url) {\n" <>
    "\t\tthis.baseurl = url;\n" <>
    "\t}\n" <>
    ms <>
    "}"

--------------------------------------------------------------------------------

generateAPI :: CommonGeneratorOptions -> AjaxReq -> T.Text
generateAPI CommonGeneratorOptions{..} Req{..} = "\t" <>
    fname <> "(" <> argsStr <> ")\n"
 <> "\t{\n"
 <> "\t\t$.ajax(\n"
 <> "\t\t\t{ url: " <> url <> "\n"
 <> "\t\t\t, success: " <> onSuccess <> "\n"
 <> dataBody
 <> reqheaders
 <> "\t\t\t, error: " <> onError <> "\n"
 <> "\t\t\t, type: '" <> decodeUtf8 method <> "'\n"
 <> "\t\t});\n"
 <> "\t}\n\n"

  where argsStr :: T.Text
        argsStr = T.intercalate ", " args

        args :: [T.Text]
        args = captures
            ++ map ( unPathSegment . _argName  . _queryArgName) queryparams
            ++ body
            ++ map (toValidFunctionName
                   . (<>) "header"
                   . unPathSegment . _argName . _headerArg
                   ) hs
            ++ [onSuccess, onError]

        captures :: [T.Text]
        captures = map (unPathSegment. _argName . captureArg)
                 . filter isCapture
                 $ _path _reqUrl

        hs = _reqHeaders

        queryparams = _queryStr _reqUrl

        body = if isJust _reqBody
                 then [requestBody]
                 else []

        onSuccess = successCallback
        onError = errorCallback

        dataBody =
          if isJust _reqBody
            then "\t\t\t, data: JSON.stringify(body)\n" <>
                 "\t\t\t, contentType: 'application/json'\n"
            else ""

        reqheaders =
          if null hs
            then ""
            else "\t\t\t, headers: { " <> headersStr <> " }\n"

          where
            headersStr :: T.Text
            headersStr = T.intercalate ", " $ map headerStr hs


            headerStr header = "\"" <>
              (unPathSegment . _argName)  (_headerArg header) <>
              "\": " <> toJSHeader header

        namespace :: T.Text
        namespace = if moduleName == ""
                       then "var "
                       else moduleName <> "."

        fname :: T.Text
        fname = functionNameBuilder _reqFuncName

        method = _reqMethod

        url :: T.Text
        url = "this.baseurl + " <> if url' == "'" then "'/'" else url'

        url' :: T.Text
        url' = "'"
           <> urlPrefix
           <> urlArgs
           <> queryArgs

        urlArgs :: T.Text
        urlArgs = jsSegments $ _path _reqUrl

        queryArgs = addQueryParams queryparams

--args :: Url f -> T.Text
args = jsSegments . _path

addQueryParams :: [QueryArg f] -> T.Text
addQueryParams ps
    | null ps = ""
    | otherwise = " + '?" <> jsParams ps

--------------------------------------------------------------------------------
