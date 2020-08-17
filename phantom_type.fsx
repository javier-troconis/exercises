type Bytes<'a> = Bytes of byte array

type Ascii = Ascii
type Utf8 = Utf8

let ascii:string -> Ascii Bytes = System.Text.Encoding.ASCII.GetBytes >> Bytes
let uf8:string -> Utf8 Bytes = System.Text.Encoding.UTF8.GetBytes >> Bytes

let a = ascii "a"
let b = uf8 "a"

a = b


