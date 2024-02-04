with Ada.Calendar;
with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO.Text_Streams;
with Web.HTML;
with Web.Lock_Files;
with Web.Producers;
with Web.RSS;
pragma Unreferenced (Web.RSS);
procedure test_web is
	use type Ada.Calendar.Time;
	Verbose : Boolean := False;
	HT : Character renames Ada.Characters.Latin_1.HT;
	Template_Source : constant String := "template.html";
	Template_Cache : constant String :=
		Ada.Directories.Compose (
			Ada.Directories.Containing_Directory (Ada.Command_Line.Command_Name),
			"template-cache.dat");
	Lock_Name : constant String :=
		Ada.Directories.Compose (
			Ada.Directories.Containing_Directory (Ada.Command_Line.Command_Name),
			"lockfile");
	Output_Name : constant String :=
		Ada.Directories.Compose (
			Ada.Directories.Containing_Directory (Ada.Command_Line.Command_Name),
			"out");
	procedure Check_Line (F : in Ada.Text_IO.File_Type; S : in String) is
		Line : constant String := Ada.Text_IO.Get_Line (F);
	begin
		if Verbose then
			Ada.Text_IO.Put_Line (Line);
		end if;
		pragma Assert (Line = S);
	end Check_Line;
	procedure Try_Produce (By_Iterator : Boolean) is
		Lock : Web.Lock_Files.Lock_Type := Web.Lock_Files.Lock (Lock_Name);
		Output_File : Ada.Text_IO.File_Type;
		Output : Ada.Text_IO.Text_Streams.Stream_Access;
		Template_Source_File : Ada.Streams.Stream_IO.File_Type;
		Template_Cache_File : Ada.Streams.Stream_IO.File_Type;
		Is_Cache : Boolean;
	begin
		Ada.Text_IO.Create (Output_File, Name => Output_Name);
		Output := Ada.Text_IO.Text_Streams.Stream (Output_File);
		Web.Header_Content_Type (Output, Web.Application_XHTML_XML);
		Web.Header_Break (Output);
		Ada.Streams.Stream_IO.Open (
			Template_Source_File,
			Ada.Streams.Stream_IO.In_File,
			Name => Template_Source);
		declare
			procedure Handler (
				Output : not null access Ada.Streams.Root_Stream_Type'Class;
				Tag : in String;
				Contents : in Web.Producers.Template) is
			begin
				if Tag = "title" then
					Web.HTML.Write_In_HTML (Output, Web.HTML.HTML4, "<<sample>>");
				elsif Tag = "generator" then
					Web.HTML.Write_Begin_Attribute (Output, "content");
					if By_Iterator then
						Web.HTML.Write_In_Attribute (Output, Web.HTML.XHTML1, "by iterator");
					else -- by closure
						Web.HTML.Write_In_Attribute (Output, Web.HTML.XHTML1, "by closure");
					end if;
					Web.HTML.Write_End_Attribute (Output);
				elsif Tag = "href" then
					Web.HTML.Write_Begin_Attribute (Output, "href");
					Web.HTML.Write_In_Attribute (
						Output,
						Web.HTML.XHTML1,
						"http://www.google.co.jp/search?q=1%2B1");
					Web.HTML.Write_End_Attribute (Output);
				elsif Tag = "is_cache" then
					if Is_Cache then
						if By_Iterator then
							for I in Web.Producers.Produce (Output, Contents, "true") loop
								raise Web.Producers.Data_Error;
							end loop;
						else -- by closure
							Web.Producers.Produce (Output, Contents, "true");
						end if;
					else
						if By_Iterator then
							for I in Web.Producers.Produce (Output, Contents, "false") loop
								raise Web.Producers.Data_Error;
							end loop;
						else -- by closure
							Web.Producers.Produce (Output, Contents, "false");
						end if;
					end if;
				else
					raise Web.Producers.Data_Error;
				end if;
			end Handler;
			Template : Web.Producers.Template := Web.Producers.Read (
				Ada.Streams.Stream_IO.Stream (Template_Source_File),
				Ada.Streams.Stream_Element_Count (
					Ada.Streams.Stream_IO.Size (Template_Source_File)),
				Parsing => False); -- suppress parsing now
		begin
			if Ada.Directories.Exists (Template_Cache)
				and then Ada.Directories.Modification_Time (Template_Cache) >
					Ada.Directories.Modification_Time (Template_Source)
			then
				Is_Cache := True;
				-- read parsed-structure from cache file
				Ada.Streams.Stream_IO.Open (Template_Cache_File, Ada.Streams.Stream_IO.In_File,
					Name => Template_Cache);
				Web.Producers.Read_Parsed_Information (
					Ada.Streams.Stream_IO.Stream (Template_Cache_File),
					Template);
				Ada.Streams.Stream_IO.Close (Template_Cache_File);
			else
				Is_Cache := False;
				Web.Producers.Parse (Template);
				-- save parsed-structure to cache file
				Ada.Streams.Stream_IO.Create (
					Template_Cache_File,
					Ada.Streams.Stream_IO.Out_File,
					Name => Template_Cache);
				Web.Producers.Write_Parsed_Information (
					Ada.Streams.Stream_IO.Stream (Template_Cache_File),
					Template);
				Ada.Streams.Stream_IO.Close (Template_Cache_File);
			end if;
			if By_Iterator then
				for I in Web.Producers.Produce (Output, Template) loop
					Handler (Output, Web.Producers.Tag (I), Web.Producers.Contents (I));
				end loop;
			else -- by closure
				Web.Producers.Produce (Output, Template, Handler => Handler'Access);
			end if;
		end;
		Ada.Streams.Stream_IO.Close (Template_Source_File);
		Ada.Text_IO.Close (Output_File);
		-- check the content
		Ada.Text_IO.Open (Output_File, Ada.Text_IO.In_File, Name => Output_Name);
		Check_Line (Output_File, "content-type: application/xhtml+xml");
		Check_Line (Output_File, "");
		Check_Line (Output_File, "<?xml version=""1.0"" encoding=""UTF-8""?>");
		Check_Line (
			Output_File,
			"<!DOCTYPE html PUBLIC ""-//W3C//DTD XHTML 1.1//EN""");
		Check_Line (
			Output_File,
			HT & """http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"">");
		Check_Line (
			Output_File,
			"<html xmlns=""http://www.w3.org/1999/xhtml"" xml:lang=""en"">");
		Check_Line (Output_File, "<head>");
		Check_Line (Output_File, HT & "<title>&lt;&lt;sample&gt;&gt;</title>");
		if By_Iterator then
			Check_Line (
				Output_File,
				HT & "<meta name=""GENERATOR"" content=""by iterator"" />");
		else
			Check_Line (
				Output_File,
				HT & "<meta name=""GENERATOR"" content=""by closure"" />");
		end if;
		Check_Line (Output_File, "</head>");
		Check_Line (Output_File, "<body>");
		Check_Line (Output_File, HT & "<p>");
		Check_Line (
			Output_File,
			HT & HT & "<a href=""http://www.google.co.jp/search?q=1%2B1"">1 + 1 = ?"
				& "</a><br/>");
		if Is_Cache then
			Check_Line (Output_File, HT & HT & "this is cache.");
		else
			Check_Line (Output_File, HT & HT & "this is parsed template.");
		end if;
		Check_Line (Output_File, HT & "</p>");
		Check_Line (Output_File, "</body>");
		Check_Line (Output_File, "</html>");
		pragma Assert (Ada.Text_IO.End_Of_File (Output_File));
		Ada.Text_IO.Close (Output_File);
	end Try_Produce;
begin
	for I in 1 .. Ada.Command_Line.Argument_Count loop
		if Ada.Command_Line.Argument (I) = "-v" then
			Verbose := True;
		end if;
	end loop;
	for By_Iterator in Boolean loop
		for Cached in Boolean loop
			if not Cached and then Ada.Directories.Exists (Template_Cache) then
				Ada.Directories.Delete_File (Template_Cache);
			end if;
			Try_Produce (By_Iterator);
		end loop;
	end loop;
	pragma Assert (not Ada.Directories.Exists (Lock_Name));
	-- finish
	Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error.all, "ok");
end test_web;
