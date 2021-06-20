renderJValue :: JValue -> Doc
renderPair (JBool True) = text "true"
renderPair (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
