-- <link rel="alternate" type="application/rss+xml" title="RSS2.0" href="*" />
package Web.RSS is
	
	procedure RSS_Start (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Title : in String;
		Description : in String;
		Link : in String);
	
	procedure RSS_Item (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Title : in String;
		Description : in String;
		Link : in String);
	
	procedure RSS_End (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class);
	
end Web.RSS;
