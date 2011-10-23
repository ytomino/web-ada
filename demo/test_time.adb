with Ada.Calendar;
with Ada.Text_IO;
with Web;
procedure test_time is
	S : constant String := Web.Image (Ada.Calendar.Clock);
begin
	Ada.Text_IO.Put_Line (S);
	if S /= Web.Image (Web.Value (S)) then
		raise Program_Error;
	end if;
end test_time;
