module type SERIALIZER = sig
  val serialize : 'a -> bytes
end
