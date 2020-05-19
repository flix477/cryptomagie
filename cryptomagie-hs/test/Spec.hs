import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Foldable (for_)

import Base64 (encode)

inputs =
  [ ("a", "YQ==")
  , ("Ml", "TWw=")
  , ("boi", "Ym9p")
  -- i wanted to try the whole bee movie script but it's quite the one-liner
  -- so have this instead
  , ("Did you ever hear the tragedy of Darth Plagueis The Wise? I thought not. It's not a story the Jedi would tell you. It's a Sith legend. Darth Plagueis was a Dark Lord of the Sith, so powerful and so wise he could use the Force to influence the midichlorians to create life... He had such a knowledge of the dark side that he could even keep the ones he cared about from dying. The dark side of the Force is a pathway to many abilities some consider to be unnatural. He became so powerful... the only thing he was afraid of was losing his power, which eventually, of course, he did. Unfortunately, he taught his apprentice everything he knew, then his apprentice killed him in his sleep. Ironic. He could save others from death, but not himself.", "RGlkIHlvdSBldmVyIGhlYXIgdGhlIHRyYWdlZHkgb2YgRGFydGggUGxhZ3VlaXMgVGhlIFdpc2U/IEkgdGhvdWdodCBub3QuIEl0J3Mgbm90IGEgc3RvcnkgdGhlIEplZGkgd291bGQgdGVsbCB5b3UuIEl0J3MgYSBTaXRoIGxlZ2VuZC4gRGFydGggUGxhZ3VlaXMgd2FzIGEgRGFyayBMb3JkIG9mIHRoZSBTaXRoLCBzbyBwb3dlcmZ1bCBhbmQgc28gd2lzZSBoZSBjb3VsZCB1c2UgdGhlIEZvcmNlIHRvIGluZmx1ZW5jZSB0aGUgbWlkaWNobG9yaWFucyB0byBjcmVhdGUgbGlmZS4uLiBIZSBoYWQgc3VjaCBhIGtub3dsZWRnZSBvZiB0aGUgZGFyayBzaWRlIHRoYXQgaGUgY291bGQgZXZlbiBrZWVwIHRoZSBvbmVzIGhlIGNhcmVkIGFib3V0IGZyb20gZHlpbmcuIFRoZSBkYXJrIHNpZGUgb2YgdGhlIEZvcmNlIGlzIGEgcGF0aHdheSB0byBtYW55IGFiaWxpdGllcyBzb21lIGNvbnNpZGVyIHRvIGJlIHVubmF0dXJhbC4gSGUgYmVjYW1lIHNvIHBvd2VyZnVsLi4uIHRoZSBvbmx5IHRoaW5nIGhlIHdhcyBhZnJhaWQgb2Ygd2FzIGxvc2luZyBoaXMgcG93ZXIsIHdoaWNoIGV2ZW50dWFsbHksIG9mIGNvdXJzZSwgaGUgZGlkLiBVbmZvcnR1bmF0ZWx5LCBoZSB0YXVnaHQgaGlzIGFwcHJlbnRpY2UgZXZlcnl0aGluZyBoZSBrbmV3LCB0aGVuIGhpcyBhcHByZW50aWNlIGtpbGxlZCBoaW0gaW4gaGlzIHNsZWVwLiBJcm9uaWMuIEhlIGNvdWxkIHNhdmUgb3RoZXJzIGZyb20gZGVhdGgsIGJ1dCBub3QgaGltc2VsZi4=")
  ]

runTest :: (String -> String) -> (String, String) -> IO ()
runTest f (input, expected) =
  let output = f input
  in if output == expected
    then putStrLn "Passed"
    else putStrLn $ "error, " <> input <> " -> " <> output <> " isn't equal to " <> expected

encodeString :: String -> String
encodeString
  = encode
  . encodeUtf8
  . pack

main :: IO ()
main =
  for_ inputs f
  where f = runTest encodeString
