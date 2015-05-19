val abap =
  """
    |    DATA api_data TYPE bapi_doc_draw2 .
    |    DATA api_key TYPE bapi_doc_keys .
    |    DATA api_update TYPE bapi_doc_drawx2 .
    |    DATA bapi_return TYPE bapiret2.
    |    DATA subject TYPE string.
    |    DATA links TYPE ttedms_bapi_doc_drad.
    |    DATA log_ids TYPE bal_t_logh.
  """.stripMargin


val data = "\\s*DATA\\s*(\\w*)\\s*TYPE\\s*(\\w*)\\s*\\.".r

val extract: PartialFunction[String, (String, String)] = {
  case data(name, typ) => (name, typ)
}

val attrs = abap split "\n" map extract.lift flatten

def methoddef(name: String, typ: String) =
  f"""
      |METHODS get_$name returning value(re_$name) type $typ.
      |METHODS set_$name importing im_$name type $typ.
   """.stripMargin

def getimpl(name:String, typ:String) =
  f"""
     |METHOD get_$name.
     |  re_$name = $name.
     |ENDMETHOD.
   """.stripMargin

def setimpl(name:String, typ:String) =
  f"""
     |METHOD set_$name.
     |  $name = im_$name.
     |ENDMETHOD.
   """.stripMargin

//Path("d:/docs/abap.output") writeStrings(
//  List(methoddef _, getimpl _, setimpl _) flatMap (attrs map _.tupled)
//  , "\n")
