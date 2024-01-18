!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummyArgCharctrstc009a.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Characteristics of DTIO interface and procedures
!*                               shall be the same as ones defined in Section 9.5.3.7.2.
!*                               - Dummy Argument Characteristics 
!*                                  - with different dummy argument names (compiler should not complain)
!*                                    and ensure iostat and iomsg still works (with formatted I/O)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1

   type base
      character(3) :: c
   end type
      
end module

program dummyArgCharctrstc009a
   use m1
   
   interface write(formatted)
      subroutine mywrite (dtvF, unitF, iotypeF, v_listF, iostatF, iomsgF)
         import base
         class(base), intent(in) :: dtvF
         integer, intent(in) :: unitF
         character(*), intent(in) :: iotypeF
         integer, intent(in) :: v_listF(:)
         integer, intent(out) :: iostatF
         character(*), intent(inout) :: iomsgF
      end subroutine
   end interface
   
    interface read(formatted)
       subroutine myread (dtv, unit, iotype, v_list, iostat, iomsg)
          import base
          class(base), intent(inout) :: dtv
          integer, intent(in) :: unit
          character(*), intent(in) :: iotype
          integer, dimension(:), intent(in) :: v_list
          integer, intent(out) :: iostat
          character(*), intent(inout) :: iomsg
       end subroutine
   end interface
   
   class(base), allocatable :: b1, b2
   integer :: stat
   character(100) :: msg  = ""
   character(100) :: msg1 = ""
   
   allocate ( b1, source = base("IBM") )
   allocate ( b2, source = base("   ") )
   
   open (1, file="dummyArgCharctrstc009a.data", access="sequential", form="formatted") 
   
   write (1,*, iostat = stat, iomsg = msg) b1
   
   if ( stat /= 0 )       error stop 1_4
   if ( msg  /= "")       error stop 2_4
   
   read  (1,*, iostat = stat, iomsg = msg) b2        !<- should have end of file error
   
   if ( stat /= -1 )      error stop 3_4
   if ( msg  == "" )      error stop 4_4
   
   rewind 1
   
   msg1 = msg
   
   read(1,*,iostat=stat, iomsg=msg1)  b2             !<- successful read shall not change iomsg but change iostat
      
   if ( b2%c /= "IBM" )   error stop 5_4

   if ( stat /= 0 )       error stop 6_4
   if ( msg1 /= msg )     error stop 7_4

   ! close the file appropriately
   
   close ( 1, status ='delete' )
   
end program

subroutine myread (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only:base
    class(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)    
    integer, intent(out) :: iostat
    character, intent(inout) :: iomsg

    character(3) :: temp
    
    read (unit, *, iostat=iostat, iomsg=iomsg ) temp
    
    dtv%c = temp
    
end subroutine

subroutine mywrite (mydtv, myunit, myiotype, myv_list, myiostat, myiomsg)
use m1, only:base
   class(base), intent(in) :: mydtv
   integer, intent(in) :: myunit
   character(*), intent(in) :: myiotype
   integer, intent(in) :: myv_list(:)
   integer, intent(out) :: myiostat
   character(*), intent(inout) :: myiomsg

   write (myunit, *, iostat=myiostat, iomsg=myiomsg ) mydtv%c
        
end subroutine
