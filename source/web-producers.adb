package body Web.Producers is
	
	procedure Append (Container : in out Node_Array_Access; Item : in Node) is
		Old : Node_Array_Access := Container;
	begin
		if Old = null then
			Container := new Node_Array'(1 => Item);
		else
			begin
				Container := new Node_Array'(Old.all & Item);
			exception
				when others =>
					Free (Old);
					raise;
			end;
			Free (Old);
		end if;
	end Append;
	
	procedure Release (Data : in out Data_Access) is
		procedure Cleanup (Item : in out Node_Array_Access) is
		begin
			if Item /= null then
				for I in Item'Range loop
					if Item (I).Nodes /= null then
						Cleanup (Item (I).Nodes);
					end if;
				end loop;
				Free (Item);
			end if;
		end Cleanup;
	begin
		if Data /= null then
			Data.Reference_Count := Data.Reference_Count - 1;
			if Data.Reference_Count = 0 then
				Free (Data.Source);
				Cleanup (Data.Root_Nodes);
				Free (Data);
			end if;
		end if;
	end Release;
	
	function Find_Part (Template : Producers.Template; Part : String)
		return Node_Array_Access is
	begin
		if Template.Nodes /= null then
			for I in Template.Nodes'Range loop
				declare
					It : Node renames Template.Nodes (I);
				begin
					if Template.Data.Source (It.Tag_First .. It.Tag_Last) = Part then
						return It.Nodes; -- OK
					end if;
				end;
			end loop;
		end if;
		raise Data_Error with """" & Part & """ was not found.";
	end Find_Part;
	
	-- bodies
	
	function Is_Empty (Object : Template) return Boolean is
	begin
		return Object.Data = null;
	end Is_Empty;
	
	function Is_Parsed (Object : Template) return Boolean is
	begin
		return Object.Nodes /= null;
	end Is_Parsed;
	
	function Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Length : Ada.Streams.Stream_Element_Count;
		Parsing : Boolean := True)
		return Template 
	is
		Source : String_Access := new String (1 .. Natural (Length));
	begin
		String'Read (Stream, Source.all);
		return Result : Template := (Ada.Finalization.Limited_Controlled with
			Data => new Data'(
				Reference_Count => 1,
				Source => Source,
				Root_Nodes => null),
			Nodes => null)
		do
			Source := null; -- free on Finalize
			if Parsing then
				Parse (Result);
			end if;
		end return;
	exception
		when others => 
			Free (Source); 
			raise;
	end Read;
	
	procedure Parse (Template : in out Producers.Template) is
	begin
		if Template.Data = null or else Template.Data.Root_Nodes /= null then
			raise Status_Error;
		else
			declare
				Source : constant not null access constant String :=
					Template.Data.Source;
				I : Positive := Source'First;
				Text_First : Positive := I;
				procedure Process (Nodes : in out Node_Array_Access; Tag : in String) is
					Text_Last : Natural;
					Tag_First : Positive;
					Tag_Last : Natural;
				begin
					while I <= Source'Last loop
						if Source (I) = '<' then
							Text_Last := I - 1;
							I := I + 1;
							if Source (I) = '/' then
								I := I + 1;
								if Source (I) = '?' then
									-- </?XXX>
									I := I + 1;
									Tag_First := I;
									while Source (I) /= '>' loop
										I := I + 1;
									end loop;
									Tag_Last := I - 1;
									I := I + 1;
									if Source (Tag_First .. Tag_Last) = Tag then
										if Text_First <= Text_Last then
											Append (
												Nodes,
												Node'(
													Text_First => Text_First,
													Text_Last => Text_Last, 
													Tag_First => 1,
													Tag_Last => 0,
													Nodes => null));
										end if;
										Text_First := I;
										return;
									end if;
								end if;
							elsif Source (I) = '?' then
								I := I + 1;
								Tag_First := I;
								loop
									case Source (I) is
										when '/' =>
											Tag_Last := I - 1;
											I := I + 1;
											if Source (I) /= '>' then
												raise Data_Error;
											end if;
											exit;
										when '>' => 
											Tag_Last := I - 1;
											exit;
										when others => null;
									end case;
									I := I + 1;
								end loop;
								I := I + 1;
								if Source (I - 2) /= '?' then
									-- <?XXX>
									declare
										Sub_Nodes : Node_Array_Access := null;
										Current_Text_First : constant Positive := Text_First;
										Current_Text_Last : constant Natural := Text_Last;
									begin
										Text_First := I;
										if Source (I - 2) /= '/' then
											Process (Sub_Nodes, Source (Tag_First .. Tag_Last));
										end if;
										Append (
											Nodes,
											Node'(
												Text_First => Current_Text_First,
												Text_Last => Current_Text_Last, 
												Tag_First => Tag_First,
												Tag_Last => Tag_Last,
												Nodes => Sub_Nodes));
									end;
								end if;
							else
								while Source (I) /= '>' loop
									if Source (I) = '"' then
										loop
											I := I + 1;
											exit when Source (I) = '"';
										end loop;
										I := I + 1;
									elsif Source (I) = '?' then
										-- <tag ?XXX>
										Text_Last := I - 1;
										I := I + 1;
										if Source (I) = '?' then
											-- <tag ?? ...>
											Append (
												Nodes,
												Node'(
													Text_First => Text_First,
													Text_Last => Text_Last, 
													Tag_First => 1,
													Tag_Last => 0,
													Nodes => null));
											loop
												I := I + 1;
												case Source (I) is
													when '/' | '>'  => exit;
													when '"' =>
														loop
															I := I + 1;
															exit when Source (I) = '"';
														end loop;
													when others => null;
												end case;
											end loop;
											Text_First := I;
										else
											Tag_First := I;
											loop
												case Source (I) is
													when ' ' | '=' | '/' | '>'  => exit;
													when others => null;
												end case;
												I := I + 1;
											end loop;
											Tag_Last := I - 1;
											Append (
												Nodes,
												Node'(
													Text_First => Text_First,
													Text_Last => Text_Last, 
													Tag_First => Tag_First,
													Tag_Last => Tag_Last,
													Nodes => null));
											Text_First := I;
										end if;
									else
										I := I + 1;
									end if;
								end loop;
							end if;
						else
							I := I + 1;
						end if;
					end loop;
				end Process;
				Nodes : Node_Array_Access := null;
			begin
				Process (Nodes, "");
				if Text_First <= Template.Data.Source'Last then
					Append (
						Nodes,
						Node'(
							Text_First => Text_First,
							Text_Last => Source'Last, 
							Tag_First => 1,
							Tag_Last => 0,
							Nodes => null));
				elsif Nodes = null then
					Nodes := new Node_Array'(1 .. 0 => <>); -- empty source file
				end if;
				Template.Data.Root_Nodes := Nodes;
				Template.Nodes := Nodes;
			end;
		end if;
	end Parse;
	
	procedure Read_Parsed_Information (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in out Producers.Template) is
	begin
		if Template.Data = null -- empty
			or else Template.Nodes /= null -- already parsed
		then
			raise Status_Error;
		else
			pragma Assert (Template.Data.Root_Nodes = null);
			declare
				procedure R (Nodes : in out Node_Array_Access) is
					Length : Integer;
				begin
					Integer'Read (Stream, Length);
					if Length > 0 then
						Nodes := new Node_Array (1 .. Length);
						for I in 1 .. Length loop
							Integer'Read (Stream, Nodes (I).Text_First);
							Integer'Read (Stream, Nodes (I).Text_Last);
							Integer'Read (Stream, Nodes (I).Tag_First);
							Integer'Read (Stream, Nodes (I).Tag_Last);
							R (Nodes (I).Nodes);
						end loop;
					end if;
				end R;
			begin
				R (Template.Data.Root_Nodes);
				if Template.Data.Root_Nodes = null then
					Template.Data.Root_Nodes := new Node_Array'(1 .. 0 => <>);
				end if;
				Template.Nodes := Template.Data.Root_Nodes;
			end;
		end if;
	end Read_Parsed_Information;
	
	procedure Write_Parsed_Information (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Producers.Template) is
	begin
		if Template.Nodes = null -- unparsed
			or else Template.Nodes /= Template.Data.Root_Nodes -- sub template
		then
			raise Status_Error;
		else
			pragma Assert (Template.Data.Root_Nodes /= null);
			declare
				procedure W (Nodes : in Node_Array_Access) is
				begin
					if Nodes /= null then
						pragma Assert (Nodes'First = 1);
						Integer'Write (Stream, Nodes'Length);
						for I in Nodes'Range loop
							Integer'Write (Stream, Nodes (I).Text_First);
							Integer'Write (Stream, Nodes (I).Text_Last);
							Integer'Write (Stream, Nodes (I).Tag_First);
							Integer'Write (Stream, Nodes (I).Tag_Last);
							W (Nodes (I).Nodes);
						end loop;
					else
						Integer'Write (Stream, 0);
					end if;
				end W;
			begin
				W (Template.Data.Root_Nodes);
			end;
		end if;
	end Write_Parsed_Information;
	
	procedure Start_Produce (
		Produce : out Produce_Type;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Producers.Template;
		Part : in String := "") is
	begin
		Produce.Output := Output.all'Unchecked_Access;
		Produce.Sub_Template.Data := Template.Data;
		Template.Data.Reference_Count := Template.Data.Reference_Count + 1;
		if Part /= "" then
			Produce.Nodes := Find_Part (Template, Part);
		else
			Produce.Nodes := Template.Nodes;
		end if;
		if Produce.Nodes /= null then
			Produce.Position := Produce.Nodes'First - 1;
			Next (Produce);
		end if;
	end Start_Produce;
	
	function More (Produce : Produce_Type) return Boolean is
	begin
		return Produce.Nodes /= null
			and then Produce.Position <= Produce.Nodes'Last
			and then Produce.Nodes (Produce.Position).Tag_Last > 0;
	end More;
	
	function Tag (Produce : Produce_Type) return String is
		It : Node renames Produce.Nodes (Produce.Position);
	begin
		return Produce.Sub_Template.Data.Source (It.Tag_First .. It.Tag_Last);
	end Tag;
	
	function Contents (Produce : Produce_Type)
		return not null access constant Template is
	begin
		return Produce.Sub_Template'Unchecked_Access;
	end Contents;
	
	procedure Next (Produce : in out Produce_Type) is
	begin
		if Produce.Nodes /= null
			and then Produce.Position <= Produce.Nodes'Last
		then
			Produce.Position := Produce.Position + 1;
			if Produce.Position <= Produce.Nodes'Last then
				declare
					It : Node renames Produce.Nodes (Produce.Position);
				begin
					String'Write (
						Produce.Output,
						Produce.Sub_Template.Data.Source (It.Text_First .. It.Text_Last));
					Produce.Sub_Template.Nodes := It.Nodes;
				end;
			end if;
		else
			raise Status_Error;
		end if;
	end Next;
	
	procedure End_Produce (Produce : in out Produce_Type) is
	begin
		Produce.Output := null;
		Produce.Nodes := null;
		Finalize (Produce.Sub_Template);
	end End_Produce;
	
	procedure Produce (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Producers.Template;
		Part : in String := "";
		Handler : access procedure (
			Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String;
			Contents : in Producers.Template) := null) 
	is
		Nodes : Node_Array_Access;
	begin
		if Part /= "" then
			Nodes := Find_Part (Template, Part);
		else
			Nodes := Template.Nodes;
		end if;
		if Nodes /= null then
			for I in Nodes'Range loop
				declare
					It : Node renames Nodes (I);
				begin
					String'Write (
						Output,
						Template.Data.Source (It.Text_First .. It.Text_Last));
					if It.Tag_First <= It.Tag_Last then
						if Handler = null then
							raise Data_Error;
						end if;
						declare
							Sub_Template : constant Producers.Template :=
								Producers.Template'(
									Ada.Finalization.Limited_Controlled with
										Data => Template.Data,
										Nodes => It.Nodes);
						begin
							Template.Data.Reference_Count :=
								Template.Data.Reference_Count + 1;
							Handler (
								Output,
								Template.Data.Source (It.Tag_First .. It.Tag_Last),
								Sub_Template);
						end;
					end if;
				end;
			end loop;
		end if;
	end Produce;
	
	procedure Generic_Produce (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Producers.Template;
		Part : in String := "";
		Handler : access procedure (
			Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String;
			Contents : in Producers.Template;
			Params : access Parameter) := null;
		Params : access Parameter) 
	is
		procedure Handle (
			Output : not null access Ada.Streams.Root_Stream_Type'Class; 
			Tag : in String; Contents : Producers.Template) is
		begin
			Handler (Output, Tag, Contents, Params);
		end Handle;
	begin
		Produce (Output, Template, Part, Handle'Access);
	end Generic_Produce;
	
	overriding procedure Finalize (Object : in out Template) is
	begin
		Release (Object.Data);
		Object.Nodes := null;
	end Finalize;
	
end Web.Producers;
