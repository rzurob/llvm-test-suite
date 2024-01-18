       ! Negative unit numbers not obtained via NEWUNIT=
       ! should be rejected

       integer, parameter :: invalid_unit_iostat = 36
       integer :: iostat
       integer :: neg_unit = -10
       character(20) c

       iostat = 0
       open(unit=neg_unit, status='scratch', iostat=iostat)
       if (iostat /= invalid_unit_iostat) then
         print *, iostat
         stop 1
       endif

       iostat = 0
       write(neg_unit, *, iostat=iostat) 'hello', neg_unit
       if (iostat /= invalid_unit_iostat) then
         print *, iostat
         stop 2
       endif

       iostat = 0
       backspace(neg_unit, iostat=iostat)
       if (iostat /= invalid_unit_iostat) then
         print *, iostat
         stop 3
       endif

       iostat = 0
       write(neg_unit, *, iostat=iostat) 'world', neg_unit
       if (iostat /= invalid_unit_iostat) then
         print *, iostat
         stop 4
       endif

       iostat = 0
       endfile(neg_unit, iostat=iostat)
       if (iostat /= invalid_unit_iostat) then
         print *, iostat
         stop 5
       endif

       iostat = 0
       rewind(neg_unit, iostat=iostat)
       if (iostat /= invalid_unit_iostat) then
         print *, iostat
         stop 6
       endif

       iostat = 0
       read(neg_unit, *, iostat=iostat) c
       if (iostat /= invalid_unit_iostat) then
         print *, iostat
         stop 7
       endif

       iostat = 0
       close(neg_unit, iostat=iostat)
       if (iostat /= 0) then
         print *, iostat
         stop 8
       endif

       end

