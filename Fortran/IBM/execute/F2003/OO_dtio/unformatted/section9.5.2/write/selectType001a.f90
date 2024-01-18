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
! %GROUP: selectType001a.f
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
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to write associate name (from select type construct)
!*                                 with unlimited polymorphic entities
!*                               Direct Access
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
   type base
      character(3) :: c = ''
   end type
   
   type, extends(base) :: child
      character(3) :: cc = ''   
   end type

end module

program selectType001a
   use m1   

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         use m1
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface
  
   ! declaration of variables
   class(*), pointer     :: b1
   class(*), allocatable :: b2
   class(*), allocatable :: b3
   integer :: stat
   character(200) :: msg =''
   character(7) :: c1, c2
   character(4) :: c3
   
   ! allocation of variables
   
   allocate ( b1, source = child('abc', 'def') )
   allocate ( b2, source = child('ghi','jkl') )
   allocate ( b3, source = base('mno') )
      
   open (unit = 1, file ='selectType001a.data', form='unformatted', access='direct', recl=10)
   
   ! unformatted I/O operations
   
   select type (b11 => b1)
      class is (base)
         write (1, iostat=stat, iomsg=msg, rec=3 )    b11     !<= write b11%c and b11%cc
         if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 1_4 
      class default
         error stop 2_4
   end select
   
   select type (b12 => b2)
      class is (child)
         write (1, iostat=stat, iomsg=msg, rec=2 )    b12     !<= write b12%c and b12%cc
         if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 3_4 
      class default
         error stop 4_4
   end select

   select type (b13 => b3)
      class is (base)
         write (1, iostat=stat, iomsg=msg, rec=1 )    b13     !<= write only b13%c
         if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 5_4 
      class default
         error stop 6_4
   end select
   
   read (1, iostat=stat, iomsg=msg, rec=1 )       c3
   if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 7_4 
   read (1, iostat=stat, iomsg=msg, rec=3 )       c1
   if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 8_4 
   read (1, iostat=stat, iomsg=msg, rec=2 )       c2
   if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 9_4 


   ! check if the values are set correctly
  
   if ( c1 /= 'abcdefX' )        error stop 10_4
   if ( c2 /= 'ghijklX' )        error stop 11_4
   if ( c3 /= 'mnoX' )           error stop 12_4
   
   ! close the file appropriately
   
   close ( 1, status ='delete' )
   
end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%c
    
    if ( iostat /= 0 ) error stop 13_4
    
    select type (dtv)
       type is (child)
          write (unit, iostat=iostat, iomsg=iomsg ) dtv%cc
    end select
    
    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "X"
    
end subroutine
