with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Web.HTML;
procedure test_encode is
	use type Ada.Strings.Unbounded.Unbounded_String;
	Verbose : Boolean := False;
	Buffer : Ada.Strings.Unbounded.Unbounded_String;
	procedure Append (S : in String) is
	begin
		Ada.Strings.Unbounded.Append (Buffer, S);
	end Append;
	procedure Check_Attribute (A, B : in String) is
		procedure Write_In_Attribute is
			new Web.HTML.Generic_Write_In_Attribute (Append, Web.HTML.XHTML);
	begin
		Buffer := Ada.Strings.Unbounded.Null_Unbounded_String;
		Write_In_Attribute (A);
		pragma Assert (Buffer = B,
			Ada.Strings.Unbounded.To_String (Buffer) & "/" & B);
		if Verbose then
			Ada.Text_IO.Put ('.');
		end if;
	end Check_Attribute;
	procedure Check_HTML (A, B : in String) is
		procedure Write_In_HTML is
			new Web.HTML.Generic_Write_In_HTML (Append, Web.HTML.XHTML);
	begin
		Buffer := Ada.Strings.Unbounded.Null_Unbounded_String;
		Write_In_HTML (A);
		pragma Assert (Buffer = B,
			Ada.Strings.Unbounded.To_String (Buffer) & "/" & B);
		if Verbose then
			Ada.Text_IO.Put ('.');
		end if;
	end Check_HTML;
begin
	for I in 1 .. Ada.Command_Line.Argument_Count loop
		if Ada.Command_Line.Argument (I) = "-v" then
			Verbose := True;
		end if;
	end loop;
	Check_Attribute ("", "");
	Check_Attribute ("*", "*");
	Check_Attribute ("""'", "&quot;&apos;");
	Check_Attribute ("*""*'*", "*&quot;*&apos;*");
	Check_Attribute ("" & ASCII.LF, "&#10;");
	Check_Attribute ("*" & ASCII.LF & "*", "*&#10;*");
	Check_Attribute (ASCII.CR & ASCII.LF, "&#10;");
	Check_Attribute ("*" & ASCII.CR & ASCII.LF & "*", "*&#10;*");
	Check_Attribute ("" & ASCII.CR, "&#10;");
	Check_Attribute ("*" & ASCII.CR & "*", "*&#10;*");
	if Verbose then
		Ada.Text_IO.New_Line;
	end if;
	Check_HTML ("", "");
	Check_HTML ("*", "*");
	Check_HTML ("&<>", "&amp;&lt;&gt;");
	Check_HTML ("*&*<*>*", "*&amp;*&lt;*&gt;*");
	Check_HTML ("" & ASCII.LF, "<br />");
	Check_HTML ("*" & ASCII.LF & "*", "*<br />*");
	Check_HTML (ASCII.CR & ASCII.LF, "<br />");
	Check_HTML ("*" & ASCII.CR & ASCII.LF & "*", "*<br />*");
	Check_HTML ("" & ASCII.CR, "<br />");
	Check_HTML ("*" & ASCII.CR & "*", "*<br />*");
	Check_HTML (" ", "&#160;");
	Check_HTML ("  ", "&#160;&#160;");
	Check_HTML (" *", "&#160;*");
	Check_HTML ("  *", "&#160;&#160;*");
	Check_HTML ("* ", "* ");
	Check_HTML ("*  ", "*&#160;&#160;");
	Check_HTML ("* *", "* *");
	Check_HTML ("*  *", "*&#160;&#160;*");
	if Verbose then
		Ada.Text_IO.New_Line;
	end if;
	pragma Debug (Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error.all, "OK"));
end test_encode;
