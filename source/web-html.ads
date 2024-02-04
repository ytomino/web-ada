package Web.HTML is
	
	-- input
	
	function Checkbox_Value (S : String) return Boolean;
	
	-- output
	
	type HTML_Version is (HTML4, HTML5, XHTML1, XHTML5, XML);
	
	generic
		with procedure Write (Item : in String);
		Version : in HTML_Version;
	procedure Generic_Write_In_HTML (
		Item : in String;
		Pre : in Boolean := False);
	
	procedure Write_In_HTML (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Version : in HTML_Version;
		Item : in String;
		Pre : in Boolean := False);
	
	generic
		with procedure Write (Item : in String);
	procedure Generic_Write_Begin_Attribute (Name : in String);
	
	procedure Write_Begin_Attribute (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Name : in String);
	
	generic
		with procedure Write (Item : in String);
		Version : in HTML_Version;
	procedure Generic_Write_In_Attribute (Item : in String);
	
	procedure Write_In_Attribute (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Version : in HTML_Version;
		Item : in String);
	
	generic
		with procedure Write (Item : in String);
	procedure Generic_Write_End_Attribute;
	
	procedure Write_End_Attribute (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class);
	
	-- write <input type="hidden" name="KEY" value="ELEMENT">...
	generic
		with procedure Write (Item : in String);
		Version : in HTML_Version;
	procedure Generic_Write_Query_In_HTML (Item : in Query_Strings);
	
	procedure Write_Query_In_HTML (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Version : in HTML_Version;
		Item : in Query_Strings);
	
	-- write ?KEY=ELEMENT&...
	generic
		with procedure Write (Item : in String);
		Version : in HTML_Version;
	procedure Generic_Write_Query_In_Attribute (Item : in Query_Strings);
	
	procedure Write_Query_In_Attribute (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Version : in HTML_Version;
		Item : in Query_Strings);
	
end Web.HTML;
