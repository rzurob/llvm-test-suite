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
! %GROUP: scale003.f
! %VERIFY: scale003.1:scale003.vf
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
!*  DESCRIPTION                : Testing: Section 10.7.5: P Editing
!*                                        Inside DTIO, P edit and see if the connection has changed scale factor temporarily for write with E and D editor
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
      real(4)      :: r1(2)
      complex(4)   :: c1(2)
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

program scale003
   use m1   

   ! declaration of variables

   class(base), allocatable  :: f1
   type(base), pointer       :: f2(:,:)
   type(base) , allocatable  :: f3
   class(base), pointer      :: f4(:)
      
   integer :: stat
   character(200) :: msg
   
   open ( 1, file = 'scale003.1', form='formatted', access='sequential' )
   
   ! allocation of variables
   
   allocate (f1, source = base(  (/1.11,2.22/), (/ (33.3, 44.4), (55.5, 66.6) /) ))
   allocate (f2(2,2), source = reshape (source=(/ base( (/5.55,66.6/), (/ (77.7, 88.8), (99.9, 11.1) /) ),  &
                                                  base( (/7.77,88.8/), (/ (99.9, 11.1), (22.2, 33.3) /) ),  &
                                                  base( (/9.99,11.1/), (/ (22.2, 33.3), (99.9, 44.4) /) ),  &
                                                  base( (/2.22,33.3/), (/ (44.4, 55.5), (66.6, 77.7) /) )   &
                                                /),shape=(/2,2/)) )
   allocate (f3, source = f2(2,2) )
   allocate (f4(2), source = (/ f1, f3 /) )
   
   ! formatted I/O operations

   write (1, *, iostat=stat, iomsg=msg)                f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 1_4
   
   write (1, *, iostat=stat, iomsg=msg)                f2   
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 2_4

   write (1, *, iostat=stat, iomsg=msg)                f3
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 3_4
   
   write (1, *, iostat=stat, iomsg=msg)                f4   
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 4_4
   
end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)   
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   write ( unit, iostat = iostat, fmt = '(1X,E10.4,-1P,1X,D10.4 )')    dtv%r1(1), dtv%r1(2)
   if ( iostat /= 0 ) error stop 10_4
   write ( unit, iostat = iostat, fmt = '(1X,E9.3E1,-3P,1X,E11.6E1,0P,1X,D9.3,1P,1X,D9.3 )')   dtv%c1(1), dtv%c1(2)  
   
   iomsg = 'dtiowrite'
   
end subroutine
