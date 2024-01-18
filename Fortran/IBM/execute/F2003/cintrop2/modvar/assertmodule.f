MODULE ASSERTMOD

  !generic interfaces for routines with overloading. 
  INTERFACE assert

     MODULE PROCEDURE  assert1_log1_i2, assert1_log2_i2,&
          assert1_log4_i2,assert1_log8_i2,assert2_log1_i2,&
          assert2_log2_i2,assert2_log4_i2,assert2_log8_i2,&
          assert3_log1_i2, assert3_log2_i2,assert3_log4_i2,&
          assert3_log8_i2,assert_v_log1_i2,assert_v_log2_i2,&
          assert_v_log4_i2,assert_v_log8_i2,&
          assert1_log1_i4, assert1_log2_i4,&
          assert1_log4_i4,assert1_log8_i4,assert2_log1_i4,&
          assert2_log2_i4,assert2_log4_i4,assert2_log8_i4,&
          assert3_log1_i4, assert3_log2_i4,assert3_log4_i4,&
          assert3_log8_i4,assert_v_log1_i4,assert_v_log2_i4,&
          assert_v_log4_i4,assert_v_log8_i4,&
          assert1_log1_i8, assert1_log2_i8,&
          assert1_log4_i8,assert1_log8_i8,assert2_log1_i8,&
          assert2_log2_i8,assert2_log4_i8,assert2_log8_i8,&
          assert3_log1_i8, assert3_log2_i8,assert3_log4_i8,&
          assert3_log8_i8,assert_v_log1_i8,assert_v_log2_i8,&
          assert_v_log4_i8,assert_v_log8_i8

  END INTERFACE

CONTAINS

!**********************************************************
!              For Integer(2)                             *
!**********************************************************
  !Routines for argument checking and error handling:

  SUBROUTINE assert1_log1_i2(n1,string,rte)
    !Report and die if any logical is false (used for arg range checking).
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(1), INTENT(IN) :: n1
    INTEGER(2) :: rte 
    if (.not. n1) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call  zzrc (int(rte,4))
    end if
  END SUBROUTINE assert1_log1_i2

  SUBROUTINE assert1_log2_i2(n1,string,rte)
    !Report and die if any logical is false (used for arg range checking).
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(2), INTENT(IN) :: n1
    INTEGER(2) :: rte 
    if (.not. n1) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc (int(rte,4))
    end if
  END SUBROUTINE assert1_log2_i2

  SUBROUTINE assert1_log4_i2(n1,string,rte)
    !Report and die if any logical is false (used for arg range checking).
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(4), INTENT(IN) :: n1
    INTEGER(2) :: rte 
    if (.not. n1) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert1_log4_i2

  SUBROUTINE assert1_log8_i2(n1,string,rte)
    !Report and die if any logical is false (used for arg range checking).
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(8), INTENT(IN) :: n1
    INTEGER(2) :: rte 
    if (.not. n1) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert1_log8_i2

  SUBROUTINE assert2_log1_i2(n1,n2,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(1), INTENT(IN) :: n1,n2
    INTEGER(2) :: rte 
    if (.not. (n1 .and. n2)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert2_log1_i2

  SUBROUTINE assert2_log2_i2(n1,n2,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(2), INTENT(IN) :: n1,n2
    INTEGER(2) :: rte 
    if (.not. (n1 .and. n2)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert2_log2_i2

  SUBROUTINE assert2_log4_i2(n1,n2,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(4), INTENT(IN) :: n1,n2
    INTEGER(2) :: rte 
    if (.not. (n1 .and. n2)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert2_log4_i2

  SUBROUTINE assert2_log8_i2(n1,n2,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(8), INTENT(IN) :: n1,n2
    INTEGER(2) :: rte 
    if (.not. (n1 .and. n2)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert2_log8_i2

  SUBROUTINE assert3_log1_i2(n1,n2,n3,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(1), INTENT(IN) :: n1,n2,n3
    INTEGER(2) :: rte
    if (.not. (n1 .and. n2 .and. n3)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert3_log1_i2

  SUBROUTINE assert3_log2_i2(n1,n2,n3,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(2), INTENT(IN) :: n1,n2,n3
    INTEGER(2) :: rte
    if (.not. (n1 .and. n2 .and. n3)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert3_log2_i2

  SUBROUTINE assert3_log4_i2(n1,n2,n3,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(4), INTENT(IN) :: n1,n2,n3
    INTEGER(2) :: rte
    if (.not. (n1 .and. n2 .and. n3)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert3_log4_i2

  SUBROUTINE assert3_log8_i2(n1,n2,n3,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(8), INTENT(IN) :: n1,n2,n3
    INTEGER(2) :: rte
    if (.not. (n1 .and. n2 .and. n3)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert3_log8_i2

  SUBROUTINE assert_v_log1_i2(n,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(1), DIMENSION(:), INTENT(IN) :: n
    INTEGER(2) :: rte
    if (.not. all(n)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert_v_log1_i2

  SUBROUTINE assert_v_log2_i2(n,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(2), DIMENSION(:), INTENT(IN) :: n
    INTEGER(2) :: rte
    if (.not. all(n)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert_v_log2_i2

  SUBROUTINE assert_v_log4_i2(n,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(4), DIMENSION(:), INTENT(IN) :: n
    INTEGER(2) :: rte
    if (.not. all(n)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert_v_log4_i2

  SUBROUTINE assert_v_log8_i2(n,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(8), DIMENSION(:), INTENT(IN) :: n
    INTEGER(2) :: rte
    if (.not. all(n)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert_v_log8_i2

!**********************************************************
!              For Integer(4)                             *
!**********************************************************
     
  !Routines for argument checking and error handling:

  SUBROUTINE assert1_log1_i4(n1,string,rte)
    !Report and die if any logical is false (used for arg range checking).
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(1), INTENT(IN) :: n1
    INTEGER(4) :: rte 
    if (.not. n1) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call  zzrc (int(rte,4))
    end if
  END SUBROUTINE assert1_log1_i4

  SUBROUTINE assert1_log2_i4(n1,string,rte)
    !Report and die if any logical is false (used for arg range checking).
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(2), INTENT(IN) :: n1
    INTEGER(4) :: rte 
    if (.not. n1) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc (int(rte,4))
    end if
  END SUBROUTINE assert1_log2_i4

  SUBROUTINE assert1_log4_i4(n1,string,rte)
    !Report and die if any logical is false (used for arg range checking).
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(4), INTENT(IN) :: n1
    INTEGER(4) :: rte 
    if (.not. n1) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert1_log4_i4

  SUBROUTINE assert1_log8_i4(n1,string,rte)
    !Report and die if any logical is false (used for arg range checking).
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(8), INTENT(IN) :: n1
    INTEGER(4) :: rte 
    if (.not. n1) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert1_log8_i4

  SUBROUTINE assert2_log1_i4(n1,n2,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(1), INTENT(IN) :: n1,n2
    INTEGER(4) :: rte 
    if (.not. (n1 .and. n2)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert2_log1_i4

  SUBROUTINE assert2_log2_i4(n1,n2,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(2), INTENT(IN) :: n1,n2
    INTEGER(4) :: rte 
    if (.not. (n1 .and. n2)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert2_log2_i4

  SUBROUTINE assert2_log4_i4(n1,n2,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(4), INTENT(IN) :: n1,n2
    INTEGER(4) :: rte 
    if (.not. (n1 .and. n2)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert2_log4_i4

  SUBROUTINE assert2_log8_i4(n1,n2,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(8), INTENT(IN) :: n1,n2
    INTEGER(4) :: rte 
    if (.not. (n1 .and. n2)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert2_log8_i4

  SUBROUTINE assert3_log1_i4(n1,n2,n3,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(1), INTENT(IN) :: n1,n2,n3
    INTEGER(4) :: rte
    if (.not. (n1 .and. n2 .and. n3)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert3_log1_i4

  SUBROUTINE assert3_log2_i4(n1,n2,n3,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(2), INTENT(IN) :: n1,n2,n3
    INTEGER(4) :: rte
    if (.not. (n1 .and. n2 .and. n3)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert3_log2_i4

  SUBROUTINE assert3_log4_i4(n1,n2,n3,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(4), INTENT(IN) :: n1,n2,n3
    INTEGER(4) :: rte
    if (.not. (n1 .and. n2 .and. n3)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert3_log4_i4

  SUBROUTINE assert3_log8_i4(n1,n2,n3,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(8), INTENT(IN) :: n1,n2,n3
    INTEGER(4) :: rte
    if (.not. (n1 .and. n2 .and. n3)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert3_log8_i4

  SUBROUTINE assert_v_log1_i4(n,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(1), DIMENSION(:), INTENT(IN) :: n
    INTEGER(4) :: rte
    if (.not. all(n)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert_v_log1_i4

  SUBROUTINE assert_v_log2_i4(n,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(2), DIMENSION(:), INTENT(IN) :: n
    INTEGER(4) :: rte
    if (.not. all(n)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert_v_log2_i4

  SUBROUTINE assert_v_log4_i4(n,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(4), DIMENSION(:), INTENT(IN) :: n
    INTEGER(4) :: rte
    if (.not. all(n)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert_v_log4_i4

  SUBROUTINE assert_v_log8_i4(n,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(8), DIMENSION(:), INTENT(IN) :: n
    INTEGER(4) :: rte
    if (.not. all(n)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert_v_log8_i4

!**********************************************************
!              For Integer(8)                             *
!**********************************************************

 
  SUBROUTINE assert1_log1_i8(n1,string,rte)
    !Report and die if any logical is false (used for arg range checking).
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(1), INTENT(IN) :: n1
    INTEGER(8) :: rte 
    if (.not. n1) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call  zzrc (int(rte,4))
    end if
  END SUBROUTINE assert1_log1_i8

  SUBROUTINE assert1_log2_i8(n1,string,rte)
    !Report and die if any logical is false (used for arg range checking).
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(2), INTENT(IN) :: n1
    INTEGER(8) :: rte 
    if (.not. n1) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc (int(rte,4))
    end if
  END SUBROUTINE assert1_log2_i8

  SUBROUTINE assert1_log4_i8(n1,string,rte)
    !Report and die if any logical is false (used for arg range checking).
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(4), INTENT(IN) :: n1
    INTEGER(8) :: rte 
    if (.not. n1) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert1_log4_i8

  SUBROUTINE assert1_log8_i8(n1,string,rte)
    !Report and die if any logical is false (used for arg range checking).
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(8), INTENT(IN) :: n1
    INTEGER(8) :: rte 
    if (.not. n1) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert1_log8_i8

  SUBROUTINE assert2_log1_i8(n1,n2,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(1), INTENT(IN) :: n1,n2
    INTEGER(8) :: rte 
    if (.not. (n1 .and. n2)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert2_log1_i8

  SUBROUTINE assert2_log2_i8(n1,n2,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(2), INTENT(IN) :: n1,n2
    INTEGER(8) :: rte 
    if (.not. (n1 .and. n2)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert2_log2_i8

  SUBROUTINE assert2_log4_i8(n1,n2,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(4), INTENT(IN) :: n1,n2
    INTEGER(8) :: rte 
    if (.not. (n1 .and. n2)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert2_log4_i8

  SUBROUTINE assert2_log8_i8(n1,n2,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(8), INTENT(IN) :: n1,n2
    INTEGER(8) :: rte 
    if (.not. (n1 .and. n2)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert2_log8_i8

  SUBROUTINE assert3_log1_i8(n1,n2,n3,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(1), INTENT(IN) :: n1,n2,n3
    INTEGER(8) :: rte
    if (.not. (n1 .and. n2 .and. n3)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert3_log1_i8

  SUBROUTINE assert3_log2_i8(n1,n2,n3,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(2), INTENT(IN) :: n1,n2,n3
    INTEGER(8) :: rte
    if (.not. (n1 .and. n2 .and. n3)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert3_log2_i8

  SUBROUTINE assert3_log4_i8(n1,n2,n3,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(4), INTENT(IN) :: n1,n2,n3
    INTEGER(8) :: rte
    if (.not. (n1 .and. n2 .and. n3)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert3_log4_i8

  SUBROUTINE assert3_log8_i8(n1,n2,n3,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(8), INTENT(IN) :: n1,n2,n3
    INTEGER(8) :: rte
    if (.not. (n1 .and. n2 .and. n3)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert3_log8_i8

  SUBROUTINE assert_v_log1_i8(n,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(1), DIMENSION(:), INTENT(IN) :: n
    INTEGER(8) :: rte
    if (.not. all(n)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert_v_log1_i8

  SUBROUTINE assert_v_log2_i8(n,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(2), DIMENSION(:), INTENT(IN) :: n
    INTEGER(8) :: rte
    if (.not. all(n)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert_v_log2_i8

  SUBROUTINE assert_v_log4_i8(n,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(4), DIMENSION(:), INTENT(IN) :: n
    INTEGER(8) :: rte
    if (.not. all(n)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert_v_log4_i8

  SUBROUTINE assert_v_log8_i8(n,string,rte)
    CHARACTER(LEN=*), INTENT(IN) :: string
    LOGICAL(8), DIMENSION(:), INTENT(IN) :: n
    INTEGER(8) :: rte
    if (.not. all(n)) then
       write (*,*) 'Error: an assertion failed with this tag:', &
            string
       call zzrc(int(rte,4)) 
    end if
  END SUBROUTINE assert_v_log8_i8

END MODULE ASSERTMOD
