!*  ===================================================================
!*
!*                               DTP - Namelist
!*
!*  DATE                       : November 25, 2008
!*
!*  PRIMARY SUBROUTINES TESTED   : Namelist with Intrinsic IO
!*  SECONDARY SUBROUTINES TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : NAMELIST
!*
!*  DESCRIPTION                :
!*  namelist-stmt is
!*
!*     NAMELIST  / namelist-group-name / namelist-group-object-list &
!*     [ [ , ] / namelist-group-name / namelist-group-object-list ] . . .
!*
!*  see defec 359460
!234567890123456789012345678901234567890123456789012345678901234567890
MODULE Mod1
      IMPLICIT NONE

      TYPE Base (k1,l1)  !(4,10)
        INTEGER, KIND :: k1
        INTEGER, LEN  :: l1

        INTEGER(k1/k1) :: Iarr(l1) = k1
        COMPLEX(k1)    :: Zarr(l1/5)   = (k1,k1)
        CHARACTER(l1)  :: Carr(l1) = 'ABCD '
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (k2,l2)  !(4,10)
        INTEGER, KIND :: k2
        INTEGER, LEN  :: l2

        TYPE(Base(k2,l2)) :: dtarr(l2/l1)
      END TYPE Child

      TYPE, EXTENDS(Child) :: NextGen (k3,l3)
        INTEGER, KIND :: k3
        INTEGER, LEN  :: l3
      END TYPE NextGen

      TYPE, EXTENDS(NextGen) :: LastGen (k4,l4)
        INTEGER, KIND :: k4
        INTEGER, LEN  :: l4
      END TYPE LastGen

      CONTAINS

      SUBROUTINE sub1(arg1, arg2, arg3)
        TYPE(Base(4,*))  :: arg1
        TYPE(LastGen(4,*,4,*,1024,*,1024,*)), OPTIONAL  :: arg2 , arg3(:)

       NAMELIST /NML1/arg1, /NML2/arg1,arg2, /NML3/arg1,arg3, /NML4/arg1,arg2,arg3

      IF ( (.not. present (arg2)) .and. (.not. present(arg3)) ) THEN
         WRITE(*, NML=NML1)
      ENDIF

      IF ( present (arg2) .and. (.not. present(arg3)) ) THEN
         WRITE(*, NML=NML2)
      ENDIF

      IF ( present (arg3) .and. (.not. present(arg2)) ) THEN
         WRITE(*, NML=NML3)
      ENDIF

      IF ( present (arg2) .and. present(arg3) ) THEN
         WRITE(*, NML=NML4)
      ENDIF

      END SUBROUTINE sub1

END MODULE Mod1
!*
PROGRAM Namelist_dummyArg07
      USE MOD1
      IMPLICIT NONE

       CLASS(Base(4,:)), ALLOCATABLE :: d1, d2, d3(:)
       integer i


       ALLOCATE(Base(4,10):: d1 )
       ALLOCATE(LastGen(4,10,4,10,1024,1024,1024,1024):: d2, d3(2) )

       call sub1(d1)

       SELECT TYPE(d2)
         TYPE IS (LastGen (4,*,4,*,1024,*,1024,*))
            d2 = LastGen (4,10,4,10,1024,1024,1024,1024) (Zarr = (9.5,9.5) , Carr = 'Niels', Iarr = 1, &
             & dtarr = Base(4,10) (Zarr = (6.6,6.6) , Carr = 'Erwin', Iarr = 6) )

            call sub1(d1,d2)

            SELECT TYPE(d3)
              TYPE IS (LastGen (4,*,4,*,1024,*,1024,*))
                d3 = (/ (LastGen (4,10,4,10,1024,1024,1024,1024) (Zarr = (7.2,7.2) , Carr = 'Bohr', Iarr = 7, &
                 & dtarr = Base(4,10) (Zarr = (8.8,8.8) , Carr = 'Shrodinger ', Iarr = 8) ), i = 1,2)  /)

                  call sub1(d1,d2,d3)

              CLASS DEFAULT
                STOP 11
            END SELECT

         CLASS DEFAULT
           STOP 10
       END SELECT

END PROGRAM Namelist_dummyArg07
