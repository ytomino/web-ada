with Ada.Calendar;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO.Text_Streams;
with Web;
with Web.Lock_Files;
with Web.Producers;
with Web.RSS;
pragma Unreferenced (Web.RSS);
procedure test_web is
	use type Ada.Calendar.Time;
	Template_Source : constant String := "template.html";
	Template_Cache : constant String := "template-cache.dat";
	Lock : Web.Lock_Files.Lock_Type := Web.Lock_Files.Lock ("lockfile");
	Output : constant not null Ada.Text_IO.Text_Streams.Stream_Access :=
		Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Output.all);
begin
	Web.Header_Content_Type (Output, Web.Text_HTML);
	Web.Header_Break (Output);
	declare
		Template_Source_File : Ada.Streams.Stream_IO.File_Type;
		Template_Cache_File : Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Open (
			Template_Source_File,
			Ada.Streams.Stream_IO.In_File,
			Name => Template_Source);
		declare
			Is_Cache : Boolean;
			procedure Handler (
				Output : not null access Ada.Streams.Root_Stream_Type'Class;
				Tag : in String;
				Contents : in Web.Producers.Template) is
			begin
				if Tag = "title" then
					Web.Write_In_HTML (Output, Web.HTML, "<<sample>>");
				elsif Tag = "href" then
					String'Write (Output, "href=""");
					Web.Write_In_Attribute (Output, Web.HTML,
						"http://www.google.co.jp/search?q=1%2B1");
					Character'Write (Output, '"');
				elsif Tag = "is_cache" then
					if Is_Cache then
						Web.Producers.Produce (Output, Contents, "true");
					else
						Web.Producers.Produce (Output, Contents, "false");
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
				Ada.Streams.Stream_IO.Open (
					Template_Cache_File,
					Ada.Streams.Stream_IO.In_File,
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
			Web.Producers.Produce (Output, Template, Handler => Handler'Access);
		end;
		Ada.Streams.Stream_IO.Close (Template_Source_File);
	end;
end test_web;
