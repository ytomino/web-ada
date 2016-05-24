with Ada.Finalization;
with Ada.IO_Exceptions;
with Ada.Streams;
with Ada.Unchecked_Deallocation;
package Web.Producers is
	
	type Template is tagged limited private;
	
--	subtype Not_Empty_Template is Template
--		with
--			Dynamic_Predicate => not Is_Empty (Not_Empty_Template)
--			Predicate_Failure => Status_Error;
--	subtype Not_Parsed_Template is Not_Empty_Template
--		with
--			Dynamic_Predicate => not Is_Parsed (Not_Parsed_Template)
--			Predicate_Failure => Status_Error;
--	subtype Parsed_Template is Not_Empty_Template
--		with
--			Dynamic_Predicate => Is_Parsed (Parsed_Template)
--			Predicate_Failure => Status_Error;
	
	function Is_Empty (Object : Template) return Boolean;
	function Is_Parsed (Object : Template) return Boolean;
	
	function Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Length : Ada.Streams.Stream_Element_Count;
		Parsing : Boolean := True)
		return Template;
	
	procedure Parse (
		Template : in out Producers.Template); -- Not_Parsed_Template
	
	procedure Read_Parsed_Information (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in out Producers.Template); -- Not_Parsed_Template
	procedure Write_Parsed_Information (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Producers.Template); -- Parsed_Template
	
	-- Producing by while loop:
	
	type Produce_Type is limited private;
	procedure Start_Produce (
		Produce : out Produce_Type;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Producers.Template; -- Parsed_Template
		Part : in String := "");
	function More (Produce : Produce_Type) return Boolean;
	function Tag (Produce : Produce_Type) return String;
	function Contents (Produce : Produce_Type)
		return not null access constant Template;
	procedure Next (Produce : in out Produce_Type);
	procedure End_Produce (Produce : in out Produce_Type);
	
	-- Producing by closure:
	
	procedure Produce (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Producers.Template; -- Parsed_Template
		Part : in String := "";
		Handler : access procedure (
			Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String;
			Contents : in Producers.Template) := null);
	
	generic
		type Parameter (<>) is limited private;
	procedure Generic_Produce (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Producers.Template; -- Parsed_Template
		Part : in String := "";
		Handler : access procedure (
			Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String;
			Contents : in Producers.Template;
			Params : access Parameter) := null;
		Params : access Parameter);
	
	-- Exceptions:
	
	Status_Error : exception
		renames Ada.IO_Exceptions.Status_Error;
	Data_Error : exception
		renames Ada.IO_Exceptions.Data_Error;
	
private
	
	type String_Access is access String;
	procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);
	
	type Node_Array (<>);
	type Node_Array_Access is access Node_Array;
	type Node is record
		Text_First : Positive;
		Text_Last : Natural;
		Tag_First : Positive;
		Tag_Last : Natural;
		Nodes : Node_Array_Access;
	end record;
	type Node_Array is array (Positive range <>) of Node;
	
	procedure Free is
		new Ada.Unchecked_Deallocation (Node_Array, Node_Array_Access);
	
	type Data is limited record
		Reference_Count : Integer;
		Source : String_Access;
		Root_Nodes : Node_Array_Access;
	end record;
	
	type Data_Access is access Data;
	
	procedure Free is new Ada.Unchecked_Deallocation (Data, Data_Access);
	
	type Template is limited new Ada.Finalization.Limited_Controlled with record
		Data : Data_Access := null;
		Nodes : Node_Array_Access := null;
	end record;
	
	overriding procedure Finalize (Object : in out Template);
	
	type Produce_Type is limited record
		Output : access Ada.Streams.Root_Stream_Type'Class;
		Nodes : Node_Array_Access;
		Position : Natural;
		Sub_Template : aliased Template;
	end record;
	
end Web.Producers;
