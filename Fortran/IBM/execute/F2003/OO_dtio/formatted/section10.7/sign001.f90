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
! %GROUP: sign001.f
! %VERIFY: sign001.1:sign001.vf
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
!*  DESCRIPTION                : Testing: Section 10.7.4: SS,SP, and S Editing
!*                                        Try these descriptors inside write DTIO procedures with I, F editors
!*                                        
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

   type :: base 
      integer(4):: i1
      real(4)   :: r1
      complex(4):: c1      
   end type
      
   interface write(formatted)
      subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface

end module

program sign001
   use m1   

   ! declaration of variables

   class(base), allocatable  :: f1
   type(base), pointer       :: f2(:,:)
   type(base) , allocatable  :: f3
   class(base), pointer      :: f4(:)
      
   integer :: stat
   character(200) :: msg
   
   open ( 1, file = 'sign001.1', form='formatted', access='sequential' )
   
   ! allocation of variables
   
   allocate (f1, source = base(-1, 2.0, (3.0, -4.0) ))
   allocate (f2(2,2), source = reshape (source=(/base(1, -2.0, (-3.0, +4.0) ),base(-5, +6.0, (7.0, -8.0) ),  &
                                                 base(+9, +10.11, (+12.13, +14.15) ), base(-6, 17.1819, (20.2122, 23.2425) ) /),shape=(/2,2/)) )
   allocate (f3, source = f2(2,2) )
   allocate (f4(2), source = (/ f1, f3 /) )
   
   ! formatted I/O operations

   write (1, *, iostat=stat, iomsg=msg)                f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 1_4
   
   write (1, *, iostat=stat, iomsg=msg)                f2   
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 2_4

   associate ( f13 => f3 )
      write (1, *, iostat=stat, iomsg=msg)                f13   
      if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 3_4
   end associate
   
   select type ( f4 )
      type is (base)
         write (1, *, iostat=stat, iomsg=msg)                f4   
         if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 4_4
   end select
   
   
end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)   
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   write (unit, "(SS,I4,1X,SP,F8.4,1X,S,F8.3,1X,SP,F8.3)", iostat=iostat )      dtv%i1, dtv%r1, dtv%c1
   
   iomsg = 'dtiowrite'
   
end subroutine
