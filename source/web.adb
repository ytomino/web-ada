with Ada.Calendar.Time_Zones;
with Ada.Environment_Variables;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed;
package body Web is
	use type Ada.Calendar.Day_Duration;
	use type String_Maps.Cursor;
	
	Month_T : constant String := "JanFebMarAprMayJunJulAugSepOctNovDec";
	Day_T : constant String := "MonTueWedThuFriSatSun";
	
	function Environment_Variables_Value (Name : String) return String is
	begin
		if Ada.Environment_Variables.Exists (Name) then
			return Ada.Environment_Variables.Value (Name);
		else
			return "";
		end if;
	end Environment_Variables_Value;
	
	procedure Header_Cookie_Internal (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Cookie : in Web.Cookie;
		Expires : access constant Ada.Calendar.Time) is
	begin
		if not Cookie.Is_Empty then
			declare
				Position : String_Maps.Cursor := String_Maps.First (Cookie);
			begin
				while String_Maps.Has_Element (Position) loop
					String'Write (Stream, "set-cookie: "
						& String_Maps.Key (Position) & "="
						& Encode_URI (String_Maps.Element (Position)) & ";");
					if Expires /= null then
						String'Write (Stream, " expires=" & Image (Expires.all) & ";");
					end if;
					String'Write (Stream, Line_Break);
					Position := String_Maps.Next (Position);
				end loop;
			end;
		end if;
	end Header_Cookie_Internal;
	
	Alt_Quot : aliased constant String := "&quot;";
	Alt_Apos : aliased constant String := "&apos;";
	Alt_LF : aliased constant String := "&#10;";
	
	procedure Write_In_Attribute_Internal (
		Write : not null access procedure (Item : String);
		Version : in HTML_Version;
		Item : in String)
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
				when ''' =>
					Alt := Alt_Apos'Access;
					goto FLUSH;
				when ASCII.LF =>
					goto NEW_LINE;
				when ASCII.CR =>
					if I < Item'Last and then Item (I + 1) = ASCII.LF then
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
				Write (Item (First .. Last));
			end if;
			Write (Alt.all);
			First := I + 1;
			goto CONTINUE;
		<<CONTINUE>>
			I := I + 1;
		end loop;
		if First <= Last then
			Write (Item (First .. Last));
		end if;
	end Write_In_Attribute_Internal;
	
	Alt_Sp : aliased constant String := "&#160;";
	Alt_Amp : aliased constant String := "&amp;";
	Alt_LT : aliased constant String := "&lt;";
	Alt_GT : aliased constant String := "&gt;";
	Alt_BRO : aliased constant String := "<br>";
	Alt_BRS : aliased constant String := "<br />";
	
	procedure Write_In_HTML_Internal (
		Write : not null access procedure (Item : String);
		Version : in HTML_Version;
		Item : in String;
		Pre : in Boolean)
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
					if not Pre and then (
						Previous_In_Spaces
							or else (I < Item'Last and then Item (I + 1) = ' '))
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
				when ASCII.LF =>
					goto NEW_LINE;
				when ASCII.CR =>
					if I < Item'Last and then Item (I + 1) = ASCII.LF then
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
				Write (Item (First .. Last));
			end if;
			Write (Alt.all);
			First := I + 1;
			goto CONTINUE;
		<<CONTINUE>>
			I := I + 1;
		end loop;
		if First <= Last then
			Write (Item (First .. Last));
		end if;
	end Write_In_HTML_Internal;
	
	procedure Write_Query_In_Attribute_Internal (
		Write : not null access procedure (Item : String);
		Version : in HTML_Version;
		Item : in Query_Strings) is
	begin
		if not Item.Is_Empty then
			declare
				First : constant String_Maps.Cursor := Item.First;
				Position : String_Maps.Cursor := First;
			begin
				while String_Maps.Has_Element (Position) loop
					if Position = First then
						Write ("?");
					else
						Write ("&");
					end if;
					Write_In_Attribute_Internal (
						Write,
						Version,
						String_Maps.Key (Position));
					Write ("=");
					Write_In_Attribute_Internal (
						Write,
						Version,
						String_Maps.Element (Position));
					String_Maps.Next (Position);
				end loop;
			end;			
		end if;
	end Write_Query_In_Attribute_Internal;
	
	procedure Write_Query_In_HTML_Internal (
		Write : not null access procedure (Item : String);
		Version : in HTML_Version;
		Item : in Query_Strings)
	is
		Position : String_Maps.Cursor := Item.First;
	begin
		while String_Maps.Has_Element (Position) loop
			Write ("<input type=""hidden"" name=""");
			Write_In_Attribute_Internal (
				Write,
				Version,
				String_Maps.Key (Position));
			Write (""" value=""");
			Write_In_Attribute_Internal (
				Write,
				Version,
				String_Maps.Element (Position));
			case Version is
				when HTML => Write (""">");
				when XHTML => Write (""" />");
			end case;
			String_Maps.Next (Position);
		end loop;		
	end Write_Query_In_HTML_Internal;
	
	Begin_Attribute : constant String := "=""";
	End_Attribute : constant String := """";
	
	-- implementation of string map
	
	function Element (Map : String_Maps.Map; Key : String; Default : String := "")
		return String
	is
		Position : String_Maps.Cursor renames String_Maps.Find (Map, Key);
	begin
		if Position = String_Maps.No_Element then
			return Default;
		else
			return String_Maps.Element (Position);
		end if;
	end Element;
	
	-- implementation of time
	
	function Image (Time : Ada.Calendar.Time) return Time_Name is
		Year : Ada.Calendar.Year_Number;
		Month : Ada.Calendar.Month_Number;
		Day : Ada.Calendar.Day_Number;
		Hou : Ada.Calendar.Formatting.Hour_Number;
		Min : Ada.Calendar.Formatting.Minute_Number;
		Sec : Ada.Calendar.Formatting.Second_Number;
		Sub_Seconds : Ada.Calendar.Day_Duration;
	begin
		Ada.Calendar.Formatting.Split (
			Time,
			Year, Month, Day,
			Hou, Min, Sec, Sub_Seconds);
		return Day_Image (Ada.Calendar.Formatting.Day_Of_Week (Time)) & ", "
			& Z2_Image (Day) & " "
			& Month_Image (Month) & " "
			& Year_Image (Year) & " "
			& Z2_Image (Hou) & ":"
			& Z2_Image (Min) & ":"
			& Z2_Image (Sec) & " GMT";
	end Image;
	
	function Value (Image : String) return Ada.Calendar.Time is
	begin
		if Image'Length /= Time_Name'Length
			and then Image'Length /= Time_Name'Length + 2 -- +XXXX form
		then
			raise Constraint_Error;
		else
			declare
				F : constant Positive := Image'First;
				Day_Of_Week : Ada.Calendar.Formatting.Day_Name;
				pragma Unreferenced (Day_Of_Week);
				Year : Ada.Calendar.Year_Number;
				Month : Ada.Calendar.Month_Number;
				Day : Ada.Calendar.Day_Number;
				Hou : Ada.Calendar.Formatting.Hour_Number;
				Min : Ada.Calendar.Formatting.Minute_Number;
				Sec : Ada.Calendar.Formatting.Second_Number;
				Offset : Ada.Calendar.Time_Zones.Time_Offset;
			begin
				Day_Of_Week := Day_Value (Image (F .. F + 2));
				if Image (F + 3 .. F + 4) /= ", " then
					raise Constraint_Error;
				end if;
				Day := Natural'Value (Image (F + 5 .. F + 6));
				if Image (F + 7) /= ' ' then
					raise Constraint_Error;
				end if;
				Month := Month_Value (Image (F + 8 .. F + 10));
				if Image (F + 11) /= ' ' then
					raise Constraint_Error;
				end if;
				Year := Natural'Value (Image (F + 12 .. F + 15));
				if Image (F + 16) /= ' ' then
					raise Constraint_Error;
				end if;
				Hou := Natural'Value (Image (F + 17 .. F + 18));
				if Image (F + 19) /= ':' then
					raise Constraint_Error;
				end if;
				Min := Natural'Value (Image (F + 20 .. F + 21));
				if Image (F + 22) /= ':' then
					raise Constraint_Error;
				end if;
				Sec := Natural'Value (Image (F + 23 .. F + 24));
				if Image'Length = Time_Name'Length + 2 then
					pragma Assert (F + 30 = Image'Last);
					if Image (F + 25) /= ' ' or else Image (F + 26) /= '+' then
						raise Constraint_Error;
					end if;
					Offset := Ada.Calendar.Time_Zones.Time_Offset (
						Natural'Value (Image (F + 27 .. F + 28)) * 60
						+ Natural'Value (Image (F + 29 .. F + 30)));
				else
					pragma Assert (Image'Length = Time_Name'Length);
					pragma Assert (F + 28 = Image'Last);
					if Image (F + 25 .. F + 28) /= " GMT" then
						raise Constraint_Error;
					end if;
					Offset := 0;
				end if;
				return Ada.Calendar.Formatting.Time_Of (
					Year, Month, Day,
					Hou, Min, Sec,
					Time_Zone => Offset);
			end;
		end if;
	end Value;
	
	function Year_Image (Year : Natural) return Year_Name is
		S : constant String := Natural'Image (Year);
	begin
		pragma Assert (S (S'First) = ' ');
		return Result : Year_Name := (others => '0') do
			Result (Year_Name'Last + 1 - (S'Last - S'First) .. Year_Name'Last) :=
				S (S'First + 1 .. S'Last);
		end return;
	end Year_Image;
	
	function Month_Image (Month : Ada.Calendar.Month_Number) return Month_Name is
	begin
		return Month_T (Month * 3 - 2 .. Month * 3);
	end Month_Image;
	
	function Month_Value (S : String) return Ada.Calendar.Month_Number is
	begin
		for Month in Ada.Calendar.Month_Number loop
			if S = Month_T (Month * 3 - 2 .. Month * 3) then
				return Month;
			end if;
		end loop;
		raise Constraint_Error;
	end Month_Value;
	
	function Day_Image (Day : Ada.Calendar.Formatting.Day_Name) return Day_Name is
		I : constant Natural := Ada.Calendar.Formatting.Day_Name'Pos (Day);
	begin
		return Day_T (I * 3 + 1 .. I * 3 + 3);
	end Day_Image;
	
	function Day_Value (S : String) return Ada.Calendar.Formatting.Day_Name is
	begin
		for Day in Ada.Calendar.Formatting.Day_Name loop
			declare
				I : constant Natural := Ada.Calendar.Formatting.Day_Name'Pos (Day);
			begin
				if S = Day_T (I * 3 + 1 .. I * 3 + 3) then
					return Day;
				end if;
			end;
		end loop;
		raise Constraint_Error;
	end Day_Value;
	
	function Z2_Image (Value : Natural) return String_2 is
		S : String := Natural'Image (Value);
	begin
		if S'Length > 2 then
			return S (S'Last - 1 .. S'Last);
		else
			pragma Assert (S'Length = 2);
			S (S'First) := '0';
			return S;
		end if;
	end Z2_Image;
	
	-- implementation of input
	
	function Request_URI return String is
		Request_URI_Value : String
			renames Environment_Variables_Value (Request_URI_Variable);
		Query_String_Value : String
			renames Environment_Variables_Value (Query_String_Variable);
	begin
		if Query_String_Value'Length = 0
			or else Ada.Strings.Fixed.Index (Request_URI_Value, "?") > 0
		then
			return Request_URI_Value;
		else
			return Request_URI_Value & "?" & Query_String_Value;
		end if;
	end Request_URI;
	
	function Request_Path return String is
		Request_URI_Value : String
			renames Environment_Variables_Value (Request_URI_Variable);
		Query_Index : constant Integer :=
			Ada.Strings.Fixed.Index (Request_URI_Value, "?");
	begin
		if Query_Index > 0 then
			return Request_URI_Value (Request_URI_Value'First .. Query_Index - 1);
		else
			return Request_URI_Value;
		end if;
	end Request_Path;
	
	function Remote_Addr return String is
	begin
		return Environment_Variables_Value (Remote_Addr_Variable);
	end Remote_Addr;
	
	function Remote_Host return String is
	begin
		return Environment_Variables_Value (Remote_Host_Variable);
	end Remote_Host;
	
	function Post return Boolean is
	begin
		return Ada.Strings.Equal_Case_Insensitive (
			Environment_Variables_Value (Request_Method_Variable),
			"post");
	end Post;
	
	function Get_Post_Length return Natural is
	begin
		return Natural'Value (
			Ada.Environment_Variables.Value (Content_Length_Variable));
	exception
		when Constraint_Error => return 0;
	end Get_Post_Length;
	
	function Get_Post_Encoded_Kind return Post_Encoded_Kind is
		Content_Type_Value : String
			renames Ada.Environment_Variables.Value (Content_Type_Variable);
	begin
		if Prefixed_Case_Insensitive (
			Content_Type_Value,
			String (Content_URL_Encoded))
		then
			return URL_Encoded;
		elsif Prefixed_Case_Insensitive (
			Content_Type_Value,
			String (Content_Multipart_Form_Data))
		then
			return Multipart_Form_Data;
		else
			return Miscellany;
		end if;
	end Get_Post_Encoded_Kind;
	
	function Encode_URI (S : String) return String is
		Integer_To_Hex : constant array (0 .. 15) of Character := "0123456789abcdef";
		Result : String (1 .. S'Length * 3);
		Length : Natural := 0;
	begin
		for I in S'Range loop
			declare
				C : constant Character := S (I);
			begin
				if (C >= 'A' and C <= 'Z')
					or else (C >= 'a' and C <= 'z')
					or else (C >= '0' and C <= '9')
				then
					Length := Length + 1;
					Result (Length) := C;
				elsif C = ' ' then
					Length := Length + 1;
					Result (Length) := '+';
				else
					Length := Length + 1;
					Result (Length) := '%';
					Length := Length + 1;
					Result (Length) := Integer_To_Hex (Character'Pos (C) / 16);
					Length := Length + 1;
					Result (Length) := Integer_To_Hex (Character'Pos (C) rem 16);
				end if;
			end;
		end loop;
		return Result (1 .. Length);
	end Encode_URI;
	
	function Decode_URI (S : String) return String is
		Hex_To_Integer : constant array (Character) of Natural := (
			'0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4, 
			'5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9,
			'A' => 10, 'B' => 11, 'C' => 12, 'D' => 13, 'E' => 14, 'F' => 15,
			'a' => 10, 'b' => 11, 'c' => 12, 'd' => 13, 'e' => 14, 'f' => 15,
			others => 0);
		Result : String (1 .. S'Length);
		I : Positive;
		Length : Natural := 0;
	begin
		I := S'First;
		while I <= S'Last loop
			declare
				C : constant Character := S (I);
			begin
				if C = '+' then
					Length := Length + 1;
					Result (Length) := ' ';
					I := I + 1;
				elsif C /= '%' then
					Length := Length + 1;
					Result (Length) := C;
					I := I + 1;
				else
					I := I + 1;
					declare
						L, H : Natural := 0;
					begin
						if I <= S'Last then
							H := Hex_To_Integer (S (I));
							I := I + 1;
							if I <= S'Last then
								L := Hex_To_Integer (S (I));
								I := I + 1;
							end if;
						end if;
						Length := Length + 1;
						Result (Length) := Character'Val (L + H * 16);
					end;
				end if;
			end;
		end loop;
		return Result (1 .. Length);
	end Decode_URI;
	
	function Decode_Query_String (S : String) return Query_Strings is
		Result : Query_Strings;
		procedure Process (S : String) is
			Sep_Pos : constant Natural := Ada.Strings.Fixed.Index (S, "=");
		begin
			if Sep_Pos >= S'First then
				String_Maps.Include (
					Result,
					S (S'First .. Sep_Pos - 1),
					Decode_URI (S (Sep_Pos + 1 .. S'Last)));
			else
				String_Maps.Include (Result, S, "");
			end if;
		end Process;
		Pos : Natural := S'First;
		Next : Natural;
	begin
		Parsing : loop
			Next := Ada.Strings.Fixed.Index (S (Pos .. S'Last), "&");
			if Next = 0 then
				Next := S'Last + 1;
			end if;
			if Pos < Next then
				Process (S (Pos .. Next - 1));
			end if;
			Pos := Next + 1;
			if Pos > S'Last then
				exit Parsing;
			end if;
		end loop Parsing;
		return Result;
	end Decode_Query_String;
	
	function Get_Query_Strings return Query_Strings is
		URI : constant String := Request_URI;
		Arg_Pos : constant Natural := Ada.Strings.Fixed.Index (URI, "?");
	begin
		if Arg_Pos = 0 then
			return String_Maps.Empty_Map;
		else
			return Decode_Query_String (URI (Arg_Pos + 1 .. URI'Last));
		end if;
	end Get_Query_Strings;
	
	function Decode_Multipart_Form_Data (S : String) return Query_Strings is
		
		function New_Line (I : access Positive) return Natural is
		begin
			if S (I.all) = Character'Val (13) then
				if I.all < S'Last and then S (I.all + 1) = Character'Val (10) then
					I.all := I.all + 2;
					return 2;
				else
					I.all := I.all + 1;
					return 1;
				end if;
			elsif S (I.all) = Character'Val (10) then
				I.all := I.all + 1;
				return 1;
			else
				return 0;
			end if;
		end New_Line;
		
		function Get_String (I : access Positive) return String is
			First, Last : Positive;
		begin
			if S (I.all) = '"' then
				I.all := I.all + 1;
				First := I.all;
				while S (I.all) /= '"' loop
					I.all := I.all + 1;
				end loop;
				Last := I.all - 1;
				I.all := I.all + 1;
				return S (First .. Last);
			end if;
			return "";
		end Get_String;
		
		procedure Skip_Spaces (I : access Positive) is
		begin
			while S (I.all) = ' ' loop
				I.all := I.all + 1;
			end loop;
		end Skip_Spaces;
		
		Result : Query_Strings;
		Position : aliased Positive;
	begin
		if S (S'First) = '-' then
			Get_First_Line : for I in S'Range loop
				Position := I;
				if New_Line (Position'Access) > 0 then
					declare
						Boundary : String renames S (S'First .. I - 1);
						Content_Disposition : constant String := "content-disposition:";
						Form_Data : constant String := "form-data;";
						Name : constant String := "name=";
						File_Name : constant String := "filename=";
						Content_Type : constant String := "content-type:";
					begin
						Separating : loop
							declare
								Next : constant Natural :=
									Ada.Strings.Fixed.Index (S (Position .. S'Last), Boundary);
								Last : Positive;
							begin
								if Next = 0 then
									Last := S'Last;
								else
									Last := Next - 1;
								end if;
								if S (Last) = Character'Val (10) then
									Last := Last - 1;
								end if;
								if S (Last) = Character'Val (13) then
									Last := Last - 1;
								end if;
								if Ada.Strings.Equal_Case_Insensitive (
									S (Position .. Position + Content_Disposition'Length - 1),
									Content_Disposition)
								then
									Position := Position + Content_Disposition'Length;
									Skip_Spaces (Position'Access);
									if S (Position .. Position + Form_Data'Length - 1) = Form_Data then
										Position := Position + Form_Data'Length;
										Skip_Spaces (Position'Access);
										if S (Position .. Position + Name'Length - 1) = Name then
											Position := Position + Name'Length;
											declare
												Item_Name : String renames Get_String (Position'Access);
											begin
												if New_Line (Position'Access) > 0 then
													while New_Line (Position'Access) > 0 loop
														null;
													end loop;
													String_Maps.Include (Result, Item_Name, S (Position .. Last));
												elsif S (Position) = ';' then
													Position := Position + 1;
													Skip_Spaces (Position'Access);
													if Ada.Strings.Equal_Case_Insensitive (
														S (Position .. Position + File_Name'Length - 1),
														File_Name)
													then
														Position := Position + File_Name'Length;
														declare
															Item_File_Name : String renames Get_String (Position'Access);
															Content_Type_First, Content_Type_Last : Positive;
														begin
															if New_Line (Position'Access) > 0 then
																if Ada.Strings.Equal_Case_Insensitive (
																	S (Position .. Position + Content_Type'Length - 1),
																	Content_Type)
																then
																	Position := Position + Content_Type'Length;
																	Skip_Spaces (Position'Access);
																	Content_Type_First := Position;
																	while S (Position) > Character'Val (32) loop
																		Position := Position + 1;
																	end loop;
																	Content_Type_Last := Position - 1;
																	while New_Line (Position'Access) > 0 loop
																		null;
																	end loop;
																	String_Maps.Include (
																		Result,
																		Item_Name,
																		S (Position .. Last));
																	String_Maps.Include (
																		Result,
																		Item_Name & ":filename",
																		Item_File_Name);
																	String_Maps.Include (
																		Result,
																		Item_Name & ":content-type",
																		S (Content_Type_First .. Content_Type_Last));
																end if;
															end if;
														end;
													end if;
												end if;
											end;
										end if;
									end if;
								end if;
								exit Separating when Next = 0;
								Position := Next + Boundary'Length;
								if New_Line (Position'Access) = 0 then
									exit Separating;
								end if;
							end;
						end loop Separating;
					end;
					exit Get_First_Line;
				end if;
			end loop Get_First_Line;
		end if;
		return Result;
	end Decode_Multipart_Form_Data;
	
	function Get (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
		return Query_Strings is
	begin
		if Post then
			declare
				Input : String (1 .. Get_Post_Length);
			begin
				String'Read (Stream, Input);
				case Get_Post_Encoded_Kind is
					when URL_Encoded => 
						return Decode_Query_String (Input);
					when Multipart_Form_Data => 
						return Decode_Multipart_Form_Data (Input);
					when Miscellany => 
						return String_Maps.Empty_Map;
				end case;
			end;
		else
			return String_Maps.Empty_Map;
		end if;
	end Get;
	
	function Get_Cookie return Cookie is
		S : String renames Environment_Variables_Value (HTTP_Cookie_Variable);
		Result : Cookie;
		procedure Process (S : in String) is
			Sep_Pos : constant Natural := Ada.Strings.Fixed.Index (S, "=");
			First : Natural := S'First;
		begin
			while S (First) = ' ' loop
				First := First + 1;
				if First > S'Last then
					return;
				end if;
			end loop;
			if Sep_Pos > First then
				String_Maps.Include (
					Result,
					S (First .. Sep_Pos - 1),
					Decode_URI (S (Sep_Pos + 1 .. S'Last)));
			else
				String_Maps.Include (Result, S (First .. S'Last), "");
			end if;
		end Process;
		Pos : Natural := S'First;
		Next : Natural;
	begin
		Parsing : loop
			Next := Ada.Strings.Fixed.Index (S (Pos .. S'Last), ";");
			if Next = 0 then
				Next := S'Last + 1;
			end if;
			if Pos < Next then
				Process (S (Pos .. Next - 1));
			end if;
			Pos := Next + 1;
			if Pos > S'Last then
				exit Parsing;
			end if;
		end loop Parsing;
		return Result;
	end Get_Cookie;
	
	function Checkbox_Value (S : String) return Boolean is
	begin
		return Ada.Strings.Equal_Case_Insensitive (S, "on");
	end Checkbox_Value;
	
	function Prefixed_Case_Insensitive (S, Prefix : String) return Boolean is
	begin
		return S'Length >= Prefix'Length
			and then Ada.Strings.Equal_Case_Insensitive (
				S (S'First .. S'First + Prefix'Length - 1),
				Prefix);
	end Prefixed_Case_Insensitive;
	
	-- implementation of output
	
	procedure Header_303 (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Location : in String) is
	begin
		String'Write (Stream, "status: 303 See Other" & Line_Break & "location: ");
		String'Write (Stream, Location);
		String'Write (Stream, Line_Break);
	end Header_303;
	
	procedure Header_503 (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class) is
	begin
		String'Write (Stream, "status: 503 Service Unavailable" & Line_Break);
	end Header_503;
	
	procedure Header_Content_Type (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Content : in Mime_Type) is
	begin
		String'Write (Stream, "content-type: ");
		String'Write (Stream, String (Content));
		String'Write (Stream, Line_Break);
	end Header_Content_Type;
	
	procedure Header_Cookie (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Cookie : in Web.Cookie;
		Expires : in Ada.Calendar.Time) is
	begin
		Header_Cookie_Internal (Stream, Cookie, Expires'Unrestricted_Access);
	end Header_Cookie;
	
	procedure Header_Cookie (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Cookie : in Web.Cookie) is
	begin
		Header_Cookie_Internal (Stream, Cookie, null);
	end Header_Cookie;
	
	procedure Header_Break (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class) is
	begin
		String'Write (Stream, Line_Break);
	end Header_Break;
	
	procedure Generic_Write (Item : in String) is
	begin
		String'Write (Stream, Item);
	end Generic_Write;
	
	procedure Generic_Write_In_HTML (
		Item : in String;
		Pre : in Boolean := False)
	is
		procedure Write_2 (Item : String) renames Write;
	begin
		Write_In_HTML_Internal (Write_2'Access, Version, Item, Pre);
	end Generic_Write_In_HTML;
	
	procedure Write_In_HTML (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Version : in HTML_Version;
		Item : in String;
		Pre : in Boolean := False)
	is
		procedure Write is new Generic_Write (Stream);
	begin
		Write_In_HTML_Internal (Write'Access, Version, Item, Pre);
	end Write_In_HTML;
	
	procedure Generic_Write_Begin_Attribute (Name : in String) is
	begin
		Write (Name);
		Write (Begin_Attribute);
	end Generic_Write_Begin_Attribute;
	
	procedure Write_Begin_Attribute (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Name : in String) is
	begin
		String'Write (Stream, Name);
		String'Write (Stream, Begin_Attribute);
	end Write_Begin_Attribute;
	
	procedure Generic_Write_In_Attribute (Item : in String) is
		procedure Write_2 (Item : String) renames Write;
	begin
		Write_In_Attribute_Internal (Write_2'Access, Version, Item);
	end Generic_Write_In_Attribute;
	
	procedure Write_In_Attribute (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Version : in HTML_Version;
		Item : in String)
	is
		procedure Write is new Generic_Write (Stream);
	begin
		Write_In_Attribute_Internal (Write'Access, Version, Item);
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
		procedure Write_2 (Item : String) renames Write;
	begin
		Write_Query_In_HTML_Internal (Write_2'Access, Version, Item);
	end Generic_Write_Query_In_HTML;
	
	procedure Write_Query_In_HTML (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Version : in HTML_Version;
		Item : in Query_Strings)
	is
		procedure Write is new Generic_Write (Stream);
	begin
		Write_Query_In_HTML_Internal (Write'Access, Version, Item);
	end Write_Query_In_HTML;
	
	procedure Generic_Write_Query_In_Attribute (Item : in Query_Strings) is
		procedure Write_2 (Item : String) renames Write;
	begin
		Write_Query_In_Attribute_Internal (Write_2'Access, Version, Item);
	end Generic_Write_Query_In_Attribute;
	
	procedure Write_Query_In_Attribute (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Version : in HTML_Version;
		Item : in Query_Strings)
	is
		procedure Write is new Generic_Write (Stream);
	begin
		Write_Query_In_Attribute_Internal (Write'Access, Version, Item);
	end Write_Query_In_Attribute;
	
end Web;
