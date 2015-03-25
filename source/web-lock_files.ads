with Ada.IO_Exceptions;
private with Ada.Finalization;
package Web.Lock_Files is
	
	function Lock (
		Name : String;
		Force : Duration := 0.0;
		Timeout : Duration := 3.0;
		Retry_Interval : Duration := 1.0;
		Forced : access Boolean := null)
		return Boolean;
	
	-- raise Lock_Error when failure
	procedure Lock (
		Name : in String;
		Force : in Duration := 0.0;
		Timeout : in Duration := 3.0;
		Retry_Interval : in Duration := 1.0;
		Forced : access Boolean := null);
	
	-- delete lock file
	procedure Unlock (Name : in String);
	
	-- RAII-style locking
	
	type Lock_Type (<>) is limited private;
	pragma Unreferenced_Objects (Lock_Type);
	
	function Lock (
		Name : String;
		Force : Duration := 0.0;
		Timeout : Duration := 3.0;
		Retry_Interval : Duration := 1.0)
		return Lock_Type;
	
	function Forced (Object : Lock_Type) return Boolean;
	
	-- explicit operation
	procedure Unlock (Object : in out Lock_Type);
	
	Name_Error : exception
		renames Ada.IO_Exceptions.Name_Error;
	Use_Error : exception
		renames Ada.IO_Exceptions.Use_Error;
	
	Lock_Error : exception;
	
private
	
	type Lock_Type (Name_Length : Natural) is
		new Ada.Finalization.Limited_Controlled with
	record
		Locked : Boolean;
		Forced : aliased Boolean;
		Name : String (1 .. Name_Length);
	end record;
	
	overriding procedure Finalize (Object : in out Lock_Type);
	
end Web.Lock_Files;
