with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Command_Line;
with Ada.Text_IO;
with Web;
procedure test_time is
	use type Ada.Calendar.Time;
	use type Ada.Calendar.Time_Zones.Time_Offset;
	Verbose : Boolean := False;
	S : constant String := Web.Image (Ada.Calendar.Clock);
begin
	for I in 1 .. Ada.Command_Line.Argument_Count loop
		if Ada.Command_Line.Argument (I) = "-v" then
			Verbose := True;
		end if;
	end loop;
	if Verbose then
		Ada.Text_IO.Put_Line (S);
	end if;
	if S /= Web.Image (Web.Value (S)) then
		raise Program_Error;
	end if;
	if Web.Value ("Thu, 21 Jul 2005 14:46:19 +0900")
		/= Ada.Calendar.Formatting.Time_Of (2005, 7, 21, 14, 46, 19,
			Time_Zone => 9 * 60)
	then
		raise Program_Error;
	end if;
	-- finish
	Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error.all, "ok");
end test_time;
