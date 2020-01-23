with Ada.Calendar;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Streams;
private with Ada.Calendar.Formatting;
private with Ada.Characters.Latin_1;
package Web is
	use type Ada.Streams.Stream_Element_Offset;
	
	-- string map
	
	package String_Maps is
		new Ada.Containers.Indefinite_Ordered_Maps (String, String);
	
	function Element (
		Map : String_Maps.Map;
		Key : String;
		Default : String := "")
		return String;
	function Element (
		Map : String_Maps.Map;
		Key : String;
		Default : Ada.Streams.Stream_Element_Array := (-1 .. 0 => <>))
		return Ada.Streams.Stream_Element_Array;
	
	procedure Include (
		Map : in out String_Maps.Map;
		Key : in String;
		Item : in String)
		renames String_Maps.Include;
	procedure Include (
		Map : in out String_Maps.Map;
		Key : in String;
		Item : in Ada.Streams.Stream_Element_Array);
	
	-- time
	
	subtype Time_Name is String (1 .. 29); -- "WWW, DD-MMM-YYYY HH:MM:SS GMT"
	function Image (Time : Ada.Calendar.Time) return Time_Name;
	function Value (Image : String) return Ada.Calendar.Time;
	
	-- protocol
	
	type Protocol is new String;
	
	HTTP : constant Protocol := "http://";
	
	-- mime-type
	
	type Mime_Type is new String;
	
	Content_Multipart_Form_Data : constant Mime_Type := "multipart/form-data";
	Content_URL_Encoded : constant Mime_Type :=
		"application/x-www-form-urlencoded";
	
	Text_Plain : constant Mime_Type := "text/plain";
	Text_HTML : constant Mime_Type := "text/html";
	Text_XML : constant Mime_Type := "text/xml";
	Application_RSS_XML : constant Mime_Type := "application/rss+xml";
	
	-- server
	
	function Host return String;
	function Compose (Protocol : Web.Protocol; Host, Path : String)
		return String;
	
	-- robots
	
	type Robots_Options is record
		No_Index : Boolean := False;
		No_Follow : Boolean := False;
		No_Archive : Boolean := False;
		No_Snippet : Boolean := False;
		No_Translate : Boolean := False;
		No_Image_Index : Boolean := False;
	end record;
	pragma Pack (Robots_Options);
	
	-- input
	
	function Request_URI return String; -- including query
	function Request_Path return String; -- excluding query
	
	function Remote_Addr return String;
	function Remote_Host return String;
	
	function User_Agent return String;
	
	function Post return Boolean;
	
	type Post_Encoded_Kind is (Miscellany, URL_Encoded, Multipart_Form_Data);
	
	function Get_Post_Length return Natural;
	function Get_Post_Encoded_Kind return Post_Encoded_Kind;
	
	function Encode_URI (S : String) return String;
	function Decode_URI (S : String) return String;
	
	subtype Query_Strings is String_Maps.Map;
	function Decode_Query_String (S : String) return Query_Strings;
	function Get_Query_Strings return Query_Strings;
	
	function Decode_Multipart_Form_Data (S : String) return Query_Strings;
	function Get (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
		return Query_Strings;
	
	subtype Cookie is String_Maps.Map;
	function Get_Cookie return Cookie;
	
	function Checkbox_Value (S : String) return Boolean;
	
	-- output
	
	procedure Header_303 (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Location : in String);
	procedure Header_See_Other (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Location : in String)
		renames Header_303;
	
	procedure Header_503 (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class);
	procedure Header_Service_Unavailable (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class)
		renames Header_503;
	
	procedure Header_Content_Type (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Content : in Mime_Type);
	
	procedure Header_Cookie (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Cookie : in Web.Cookie;
		Expires : in Ada.Calendar.Time);
	procedure Header_Cookie (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Cookie : in Web.Cookie);
	
	procedure Header_X_Robots_Tag (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Options : in Robots_Options);
	
	procedure Header_Break (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class);
	
	type HTML_Version is (HTML, XHTML);
	
	generic
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
	procedure Generic_Write (Item : in String);
	
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
	
private
	
	-- time
	
	subtype Year_Name is String (1 .. 4);
	function Year_Image (Year : Natural) return Year_Name;
	
	subtype Month_Name is String (1 .. 3);
	function Month_Image (Month : Ada.Calendar.Month_Number)
		return Month_Name;
	function Month_Value (S : String) return Ada.Calendar.Month_Number;
	
	subtype Day_Name is String (1 .. 3);
	function Day_Image (Day : Ada.Calendar.Formatting.Day_Name)
		return Day_Name;
	function Day_Value (S : String) return Ada.Calendar.Formatting.Day_Name;
	
	subtype String_2 is String (1 .. 2);
	function Z2_Image (Value : Natural) return String_2;
	
	-- input
	
	Content_Length_Variable : constant String := "CONTENT_LENGTH";
	Content_Type_Variable : constant String := "CONTENT_TYPE";
	HTTP_Cookie_Variable : constant String := "HTTP_COOKIE";
	HTTP_Host_Variable : constant String := "HTTP_HOST";
	HTTP_User_Agent_Variable : constant String := "HTTP_USER_AGENT";
	Remote_Addr_Variable : constant String := "REMOTE_ADDR";
	Remote_Host_Variable : constant String := "REMOTE_HOST";
	Request_Method_Variable : constant String := "REQUEST_METHOD";
	Request_URI_Variable : constant String := "REQUEST_URI";
	Server_Name_Variable : constant String := "SERVER_NAME";
	Query_String_Variable : constant String := "QUERY_STRING";
	
	function Equal_Case_Insensitive (S, L : String) return Boolean;
	function Prefixed_Case_Insensitive (S, L_Prefix : String) return Boolean;
	
	-- output
	
	Line_Break : aliased constant String := (1 => Ada.Characters.Latin_1.LF);
	
end Web;
