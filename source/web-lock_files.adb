with Ada.Calendar;
with Ada.Directories;
package body Web.Lock_Files is
	use type Ada.Calendar.Time;
	
	function Lock (
		Name : String;
		Force : Duration := 0.0;
		Timeout : Duration := 3.0;
		Retry_Interval : Duration := 1.0;
		Forced : access Boolean := null)
		return Boolean
	is
		Gone : Duration := 0.0;
	begin
		if Forced /= null then
			Forced.all := False;
		end if;
		loop
			if Force /= 0.0 then
				declare
					Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
				begin
					if Now - Ada.Directories.Modification_Time (Name) >= Force then
						Ada.Directories.Delete_Directory (Name);
						if Forced /= null then
							Forced.all := True;
						end if;
					end if;
				exception
					when Name_Error | Use_Error => null;
				end;
			end if;
			begin
				Ada.Directories.Create_Directory (Name);
				return True;
			exception
				when Use_Error => null;
			end;
			declare
				Interval : constant Duration :=
					Duration'Min (Retry_Interval, Timeout - Gone);
			begin
				if Interval <= 0.0 then
					return False;
				end if;
				delay Interval;
				Gone := Gone + Interval;
			end;
		end loop;
	end Lock;
	
	procedure Lock (
		Name : in String;
		Force : in Duration := 0.0;
		Timeout : in Duration := 3.0;
		Retry_Interval : in Duration := 1.0;
		Forced : access Boolean := null) is
	begin
		if not Lock (
			Name,
			Force => Force,
			Timeout => Timeout,
			Retry_Interval => Retry_Interval,
			Forced => Forced)
		then
			raise Lock_Error;
		end if;
	end Lock;
	
	procedure Unlock (Name : in String)
		renames Ada.Directories.Delete_Directory;
	
	function Lock (
		Name : String;
		Force : Duration := 0.0;
		Timeout : Duration := 3.0;
		Retry_Interval : Duration := 1.0)
		return Lock_Type is
	begin
		return Result : Lock_Type :=
			(Ada.Finalization.Limited_Controlled
				with
					Name_Length => Name'Length,
					Locked => False,
					Forced => False,
					Name => Name)
		do
			Lock (
				Name,
				Force => Force,
				Timeout => Timeout,
				Retry_Interval => Retry_Interval,
				Forced => Result.Forced'Access);
			Result.Locked := True;
		end return;
	end Lock;
	
	function Forced (Object : Lock_Type) return Boolean is
	begin
		return Object.Forced;
	end Forced;
	
	procedure Unlock (Object : in out Lock_Type) is
	begin
		if Object.Locked then
			Unlock (Object.Name);
		end if;
	end Unlock;
	
	overriding procedure Finalize (Object : in out Lock_Type) is
	begin
		Unlock (Object);
	exception
		when Name_Error | Use_Error =>
			null; -- Finalize can not raise any exception
	end Finalize;
	
end Web.Lock_Files;
