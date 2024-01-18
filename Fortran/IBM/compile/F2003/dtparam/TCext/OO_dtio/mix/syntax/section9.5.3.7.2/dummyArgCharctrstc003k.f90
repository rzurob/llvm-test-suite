! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-09-09 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Characteristics of DTIO interface and procedures
!*                               shall be the same as ones defined in Section 9.5.3.7.2.
!*                               1) Dummy Argument Characteristics
!*                                  -  whether dummy arguments are assumed-shape array
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

   type base1 (kbase1_1) ! kbase1_1=4
      integer, kind :: kbase1_1
      integer(kbase1_1) :: i
   end type

end module

program dummyArgCharctrstc003k
   use m1

   interface write(formatted)
      ! write : defines array of explicit shape instead of assumed-shape
      subroutine write (dtv, unit, iotype, v_list, iostat, iomsg)
         import base1
         class(base1(4)), intent(in) :: dtv ! tcx: (4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(95:100)                 !<- shall be assumed shape array
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

    interface read(formatted)
       ! read : defines array of zero size instead of assumed-shape
       subroutine read (dtv, unit, iotype, v_list, iostat, iomsg)
          import base1
          class(base1(4)), intent(inout) :: dtv ! tcx: (4)
          integer, intent(in) :: unit
          character(*), intent(in) :: iotype
          integer, dimension(0), intent(in) :: v_list          !<- shall be assumed shape array
          integer, intent(out) :: iostat
          character(*), intent(inout) :: iomsg
       end subroutine
   end interface

end program







! Extensions to introduce derived type parameters:
! type: base1 - added parameters (kbase1_1) to invoke with (4) / declare with (4) - 2 changes
! type: base1 - added parameters (kbase1_1) to invoke with (4) / declare with (4) - 2 changes
