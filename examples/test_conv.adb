with Ada.Text_IO;
with Web;
procedure test_conv is
	procedure Check_URI (Decoded, Encoded : in String) is
	begin
		pragma Assert (Web.Encode_URI (Decoded) = Encoded);
		pragma Assert (Web.Decode_URI (Encoded) = Decoded);
		null;
	end Check_URI;
begin
	Check_URI ("", "");
	Check_URI ("-_.!~*'()", "-_.!~*'()");
	Check_URI ("?", "%3f");
	Check_URI ("??", "%3f%3f");
	Check_URI ("0 1", "0+1");
	-- finish
	Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error.all, "ok");
end test_conv;
