package body Web.RSS is
	
	procedure RSS_Start (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Title : in String;
		Description : in String;
		Link : in String) is
	begin
		String'Write (Stream,
			"<rss version=""2.0"">" &
			"<channel>" &
			"<docs>http://blogs.law.harvard.edu/tech/rss</docs>" &
			"<title>");
		Write_In_HTML (Stream, XHTML, Title);
		String'Write (Stream,
			"</title>" &
			"<description>");
		Write_In_HTML (Stream, XHTML, Description);
		String'Write (Stream,
			"</description>" &
			"<link>");
		Write_In_HTML (Stream, XHTML, Link);
		String'Write (Stream,
			"</link>");
	end RSS_Start;
	
	procedure RSS_Item (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Title : in String;
		Description : in String;
		Link : in String) is
	begin
		String'Write (Stream,
			"<item>" &
			"<title>");
		Write_In_HTML (Stream, XHTML, Title);
		String'Write (Stream,
			"</title>" &
			"<description>");
		Write_In_HTML (Stream, XHTML, Description);
		String'Write (Stream,
			"</description>" &
			"<link>");
		Write_In_HTML (Stream, XHTML, Link);
		String'Write (Stream,
			"</link>" &
			"</item>");
	end RSS_Item;
	
	procedure RSS_End (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class) is
	begin
		String'Write (Stream,
			"</channel>" &
			"</rss>");
	end RSS_End;
	
end Web.RSS;
