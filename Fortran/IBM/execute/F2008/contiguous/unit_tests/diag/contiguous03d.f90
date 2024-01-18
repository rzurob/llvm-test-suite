!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : contiguous03d.f
!*
!*  PROGRAMMER                 : David Nichols
!*  DATE                       : June 24, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS attribute - 5.3.7.2
!*
!*  DRIVER STANZA              : xlf2008
!*
!*  DESCRIPTION                : Testing proper diagnostics of
!*                               the F2008 attribute
!*                               CONTIGUOUS
!*                               Testing 5.3.7.2 CONTIGUOUS
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program contiguous03d

        ! 5.7.3.2: An object is contiguous if it is:

        type base
          integer, pointer, contiguous :: bipc(:)
        end type

        ! 1: an object with the CONTIGUOUS attribute,
        type(base), pointer, contiguous :: bpc(:)
        integer,    pointer, contiguous :: ipc(:)
        character,  pointer, contiguous :: cpc(:)
        integer,    pointer             :: ip (:)
        character,  pointer             :: cp (:)
        integer,    allocatable         :: ia (:)
        
        ! 2: a nonpointer whole array that is not assumed-shape,
        integer,   target               :: it (5)
        character, target               :: ct (5)                  

        integer                         :: one, five

        one = 1
        five = 5

        ! 3: an assumed-shape array that is argument associated
        ! with an array that is contiguous,
        call contiguous_sub(it)
        
        ! 4: an array allocated by an ALLOCATE statement,
        allocate(ia(10))
        allocate(bpc(10))

        ! 5: a pointer associated with a contiguous target, or
        ip  => ipc ! ip is now contiguous
        cp  => cpc ! cp is now contiguous

        ipc => ip ! ip and cp are contiguous due to 5.7.3.2.5
        cpc => cp
        
        ipc => it ! it and ct are contiguous due to 5.7.3.2.2
        cpc => ct 

        ! 6: a nonzero-sized array section (6.5.3) provided that:
        ! (a) its base object is contiguous,
        ipc => it(1:5:1)
        ipc => it(one:five:1)
        ipc => it(MIN(1,5):MAX(1,5):1)
        
        ! (b) it does not have a vector subscript,
        call contiguous_sub( it([1,2,3,4,5]) )

        ! (c) the elements of the section, in array element order, are
        !     a subset of the base object elements
        !     that are consecutive in array element order,
        ipc => it(1:5)
        ipc => it(one:five)
        
        ! (d) if the array is of type character and a substring-range
        !     appears, the substring-range specifies all
        !     of the characters of the parent-string (6.4.1),
        cpc => ct(1:5)
        cpc => ct(one:five)
        
        ! (e) only its final part-ref has nonzero rank, and
        !     scalar%scalar%array(section)
        ipc => bpc(1)%bipc(1:5)
        ipc => bpc(1)%bipc(one:five)
        
        ! (f) it is not the real or imaginary part (6.4.4) of an array
        !     of type complex.
        !
        ! Feature not in plan.

        contains

        subroutine contiguous_sub (A)
          ! 3: an assumed-shape array that is argument associated
          ! with an array that is contiguous,
          integer, contiguous     :: A (:)

        end subroutine

      end
