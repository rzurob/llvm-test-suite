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
! %GROUP: zerosized002.f
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
!*  DATE                       : 11/04/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Ensure zero-sized type will invoke DTIO for array(sequential access write)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type :: base
       character(0) :: i
    end type

    type :: emptybase
    end type
    
    type :: nonemptybase
       character(1) :: c = 'c'
    end type
    
end module

program zerosized002
use m
    
   interface write(unformatted)
      subroutine unformattedWrite (dtv, unit, iostat, iomsg)
      use m
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
        
      subroutine unformattedWriteZero (dtv, unit, iostat, iomsg)
      use m
         class(emptybase), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
      
      subroutine unformattedWriteNonEmpty (dtv, unit, iostat, iomsg)
      use m
         class(nonemptybase), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
      
   end interface
   
   ! declaration of variables
   
   integer :: stat
    
   character(3) :: c1
   character(9) :: c2
   character(2) :: c3, c6
   character(4) :: c4
   character(5) :: c5
   character(0) :: c7, c8, c9
   
   class(base), allocatable :: b1(:)
   class(base), pointer     :: b2(:,:)
   type (base)              :: b3(2)
   
   class(emptybase), allocatable :: e1(:,:)
   class(emptybase), pointer     :: e2(:)
   type (emptybase)              :: e3(2)
   
   class(nonemptybase), allocatable :: n1(:)
   class(nonemptybase), pointer     :: n2(:,:)
   type (nonemptybase)              :: n3(0)
   
   ! allocation of variables
   
   allocate ( b1(3), b2(3,3) )
   allocate ( e1(2,2), e2(5) )
   allocate ( n1(0), n2(0,0) )
   
   open(1, file='zerosized002.data', access='sequential', form='unformatted')
    
   write (1, iostat = stat) b1     !<- shall write "ZZZ"
   write (1, iostat = stat) b2     !<- shall write "ZZZZZZZZZ"
   write (1, iostat = stat) b3     !<- shall write "ZZ"
   
   write (1, iostat = stat) e1     !<- shall write "XXXX"
   write (1, iostat = stat) e2     !<- shall write "XXXXX"
   write (1, iostat = stat) e3     !<- shall write "XX"
   
   write (1, iostat = stat) n1     !<- shall write "" (shall not call DTIO since no effective items)
   if ( stat /= 0 )   error stop 1_4
   write (1, iostat = stat) n2     !<- shall write "" (shall not call DTIO since no effective items)
   if ( stat /= 0 )   error stop 2_4
   write (1, iostat = stat) n3     !<- shall write "" (shall not call DTIO since no effective items)
   if ( stat /= 0 )   error stop 3_4
   
   rewind 1
   
   read (1, iostat = stat)  c1
   read (1, iostat = stat)  c2
   read (1, iostat = stat)  c3
   
   read (1, iostat = stat)  c4
   read (1, iostat = stat)  c5
   read (1, iostat = stat)  c6
   
   read (1, iostat = stat)  c7
   read (1, iostat = stat)  c8
   read (1, iostat = stat)  c9
 
   ! check if the written values are correct

   if ( c1 /= "ZZZ" )         error stop 4_4
   if ( c2 /= "ZZZZZZZZZ" )   error stop 5_4
   if ( c3 /= "ZZ" )          error stop 6_4

   if ( c4 /= "XXXX" )        error stop 7_4
   if ( c5 /= "XXXXX" )       error stop 8_4
   if ( c6 /= "XX" )          error stop 9_4

   if ( c7 /= "" )            error stop 10_4
   if ( c8 /= "" )            error stop 11_4
   if ( c9 /= "" )            error stop 12_4
         
   ! close the file appropriately
   
   close ( 1, status ='delete' )
   
end program


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   ! add a mark at the end of record, so we know DTIO is used
   write (unit, iostat=iostat, iomsg=iomsg ) dtv%i
   write (unit, iostat=iostat, iomsg=iomsg ) "Z"

end subroutine


subroutine unformattedWriteZero (dtv, unit, iostat, iomsg)
use m
   class(emptybase), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
    
   ! add a mark at the end of record, so we know DTIO is used
   write (unit, iostat=iostat, iomsg=iomsg ) "X"

end subroutine

subroutine unformattedWriteNonEmpty (dtv, unit, iostat, iomsg)
use m
   class(nonemptybase), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
    
   write (unit, iomsg=iomsg ) dtv%c
   
   iostat = 997
   
end subroutine
