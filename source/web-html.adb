with System;
package body Web.HTML is
	use type String_Maps.Cursor;
	
	procedure By_Stream (Item : in String; Params : in System.Address) is
		function To_Pointer (Value : System.Address)
			return access Ada.Streams.Root_Stream_Type'Class
			with Import, Convention => Intrinsic;
	begin
		String'Write (To_Pointer (Params), Item);
	end By_Stream;
	
	Alt_Quot : aliased constant String := "&quot;";
	Alt_Amp : aliased constant String := "&amp;";
	Alt_Apos : aliased constant String := "&apos;";
	Alt_LF : aliased constant String := "&#10;";
	
	procedure Write_In_Attribute_Internal (
		Version : in HTML_Version;
		Item : in String;
		Params : in System.Address;
		Write : not null access procedure (
			Item : in String;
			Params : in System.Address))
	is
		Alt : access constant String;
		First : Positive := Item'First;
		Last : Natural := First - 1;
		I : Positive := Item'First;
	begin
		while I <= Item'Last loop
			case Item (I) is
				when '"' =>
					Alt := Alt_Quot'Access;
					goto FLUSH;
				when '&' =>
					Alt := Alt_Amp'Access;
					goto FLUSH;
				when ''' =>
					Alt := Alt_Apos'Access;
					goto FLUSH;
				when Ada.Characters.Latin_1.LF =>
					goto NEW_LINE;
				when Ada.Characters.Latin_1.CR =>
					if I < Item'Last
						and then Item (I + 1) = Ada.Characters.Latin_1.LF
					then
						goto CONTINUE; -- skip
					else
						goto NEW_LINE;
					end if;
				when others =>
					Last := I;
					goto CONTINUE;
			end case;
		<<NEW_LINE>>
			case Version is
				when HTML =>
					Alt := Line_Break'Access;
				when XHTML =>
					Alt := Alt_LF'Access;
			end case;
		<<FLUSH>>
			if First <= Last then
				Write (Item (First .. Last), Params);
			end if;
			Write (Alt.all, Params);
			First := I + 1;
			goto CONTINUE;
		<<CONTINUE>>
			I := I + 1;
		end loop;
		if First <= Last then
			Write (Item (First .. Last), Params);
		end if;
	end Write_In_Attribute_Internal;
	
	Alt_Sp : aliased constant String := "&#160;";
	Alt_LT : aliased constant String := "&lt;";
	Alt_GT : aliased constant String := "&gt;";
	Alt_BRO : aliased constant String := "<br>";
	Alt_BRS : aliased constant String := "<br />";
	
	procedure Write_In_HTML_Internal (
		Version : in HTML_Version;
		Item : in String;
		Pre : in Boolean;
		Params : in System.Address;
		Write : not null access procedure (
			Item : in String;
			Params : in System.Address))
	is
		Alt : access constant String;
		First : Positive := Item'First;
		Last : Natural := First - 1;
		In_Spaces : Boolean := True;
		Previous_In_Spaces : Boolean;
		I : Positive := Item'First;
	begin
		while I <= Item'Last loop
			Previous_In_Spaces := In_Spaces;
			In_Spaces := False;
			case Item (I) is
				when ' ' =>
					if not Pre
						and then (
							Previous_In_Spaces or else (I < Item'Last and then Item (I + 1) = ' '))
					then
						In_Spaces := True;
						Alt := Alt_Sp'Access;
						goto FLUSH;
					else
						Last := I;
						goto CONTINUE;
					end if;
				when '&' =>
					Alt := Alt_Amp'Access;
					goto FLUSH;
				when '<' =>
					Alt := Alt_LT'Access;
					goto FLUSH;
				when '>' =>
					Alt := Alt_GT'Access;
					goto FLUSH;
				when Ada.Characters.Latin_1.LF =>
					goto NEW_LINE;
				when Ada.Characters.Latin_1.CR =>
					if I < Item'Last and then Item (I + 1) = Ada.Characters.Latin_1.LF then
						goto CONTINUE; -- skip
					else
						goto NEW_LINE;
					end if;
				when others =>
					Last := I;
					goto CONTINUE;
			end case;
		<<NEW_LINE>>
			if Pre then
				Alt := Line_Break'Access;
			else
				case Version is
					when HTML =>
						Alt := Alt_BRO'Access;
					when XHTML =>
						Alt := Alt_BRS'Access;
				end case;
			end if;
		<<FLUSH>>
			if First <= Last then
				Write (Item (First .. Last), Params);
			end if;
			Write (Alt.all, Params);
			First := I + 1;
			goto CONTINUE;
		<<CONTINUE>>
			I := I + 1;
		end loop;
		if First <= Last then
			Write (Item (First .. Last), Params);
		end if;
	end Write_In_HTML_Internal;
	
	procedure Write_Query_In_Attribute_Internal (
		Version : in HTML_Version;
		Item : in Query_Strings;
		Params : in System.Address;
		Write : not null access procedure (
			Item : in String;
			Params : in System.Address)) is
	begin
		if not Item.Is_Empty then
			declare
				First : constant String_Maps.Cursor := Item.First;
				Position : String_Maps.Cursor := First;
			begin
				while String_Maps.Has_Element (Position) loop
					if Position = First then
						Write ("?", Params);
					else
						Write ("&", Params);
					end if;
					Write_In_Attribute_Internal (Version, String_Maps.Key (Position), Params,
						Write => Write);
					Write ("=", Params);
					Write_In_Attribute_Internal (Version, String_Maps.Element (Position), Params,
						Write => Write);
					String_Maps.Next (Position);
				end loop;
			end;
		end if;
	end Write_Query_In_Attribute_Internal;
	
	procedure Write_Query_In_HTML_Internal (
		Version : in HTML_Version;
		Item : in Query_Strings;
		Params : in System.Address;
		Write : not null access procedure (
			Item : in String;
			Params : in System.Address))
	is
		Position : String_Maps.Cursor := Item.First;
	begin
		while String_Maps.Has_Element (Position) loop
			Write ("<input type=""hidden"" name=""", Params);
			Write_In_Attribute_Internal (Version, String_Maps.Key (Position), Params,
				Write => Write);
			Write (""" value=""", Params);
			Write_In_Attribute_Internal (Version, String_Maps.Element (Position), Params,
				Write => Write);
			case Version is
				when HTML =>
					Write (""">", Params);
				when XHTML =>
					Write (""" />", Params);
			end case;
			String_Maps.Next (Position);
		end loop;
	end Write_Query_In_HTML_Internal;
	
	Begin_Attribute : constant String := "=""";
	End_Attribute : constant String := """";
	
	-- implementation of input
	
	function Checkbox_Value (S : String) return Boolean is
	begin
		return Equal_Case_Insensitive (S, L => "on");
	end Checkbox_Value;
	
	-- implementation of output
	
	procedure Generic_Write_In_HTML (
		Item : in String;
		Pre : in Boolean := False)
	is
		procedure By_Callback (Item : in String; Params : in System.Address) is
			pragma Unreferenced (Params);
		begin
			Write (Item);
		end By_Callback;
	begin
		Write_In_HTML_Internal (Version, Item, Pre, System.Null_Address,
			Write => By_Callback'Access);
	end Generic_Write_In_HTML;
	
	procedure Write_In_HTML (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Version : in HTML_Version;
		Item : in String;
		Pre : in Boolean := False)
	is
		function To_Address (Value : access Ada.Streams.Root_Stream_Type'Class)
			return System.Address
			with Import, Convention => Intrinsic;
	begin
		Write_In_HTML_Internal (Version, Item, Pre, To_Address (Stream),
			Write => By_Stream'Access);
	end Write_In_HTML;
	
	procedure Generic_Write_Begin_Attribute (Name : in String) is
	begin
		Write ((1 => ' '));
		Write (Name);
		Write (Begin_Attribute);
	end Generic_Write_Begin_Attribute;
	
	procedure Write_Begin_Attribute (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Name : in String) is
	begin
		Character'Write (Stream, ' ');
		String'Write (Stream, Name);
		String'Write (Stream, Begin_Attribute);
	end Write_Begin_Attribute;
	
	procedure Generic_Write_In_Attribute (Item : in String) is
		procedure By_Callback (Item : in String; Params : in System.Address) is
			pragma Unreferenced (Params);
		begin
			Write (Item);
		end By_Callback;
	begin
		Write_In_Attribute_Internal (Version, Item, System.Null_Address,
			Write => By_Callback'Access);
	end Generic_Write_In_Attribute;
	
	procedure Write_In_Attribute (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Version : in HTML_Version;
		Item : in String)
	is
		function To_Address (Value : access Ada.Streams.Root_Stream_Type'Class)
			return System.Address
			with Import, Convention => Intrinsic;
	begin
		Write_In_Attribute_Internal (Version, Item, To_Address (Stream),
			Write => By_Stream'Access);
	end Write_In_Attribute;
	
	procedure Generic_Write_End_Attribute is
	begin
		Write (End_Attribute);
	end Generic_Write_End_Attribute;
	
	procedure Write_End_Attribute (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class) is
	begin
		String'Write (Stream, End_Attribute);
	end Write_End_Attribute;
	
	procedure Generic_Write_Query_In_HTML (Item : in Query_Strings) is
		procedure By_Callback (Item : in String; Params : in System.Address) is
			pragma Unreferenced (Params);
		begin
			Write (Item);
		end By_Callback;
	begin
		Write_Query_In_HTML_Internal (Version, Item, System.Null_Address,
			Write => By_Callback'Access);
	end Generic_Write_Query_In_HTML;
	
	procedure Write_Query_In_HTML (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Version : in HTML_Version;
		Item : in Query_Strings)
	is
		function To_Address (Value : access Ada.Streams.Root_Stream_Type'Class)
			return System.Address
			with Import, Convention => Intrinsic;
	begin
		Write_Query_In_HTML_Internal (Version, Item, To_Address (Stream),
			Write => By_Stream'Access);
	end Write_Query_In_HTML;
	
	procedure Generic_Write_Query_In_Attribute (Item : in Query_Strings) is
		procedure By_Callback (Item : in String; Params : in System.Address) is
			pragma Unreferenced (Params);
		begin
			Write (Item);
		end By_Callback;
	begin
		Write_Query_In_Attribute_Internal (Version, Item, System.Null_Address,
			Write => By_Callback'Access);
	end Generic_Write_Query_In_Attribute;
	
	procedure Write_Query_In_Attribute (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Version : in HTML_Version;
		Item : in Query_Strings)
	is
		function To_Address (Value : access Ada.Streams.Root_Stream_Type'Class)
			return System.Address
			with Import, Convention => Intrinsic;
	begin
		Write_Query_In_Attribute_Internal (Version, Item, To_Address (Stream),
			Write => By_Stream'Access);
	end Write_Query_In_Attribute;
	
end Web.HTML;
