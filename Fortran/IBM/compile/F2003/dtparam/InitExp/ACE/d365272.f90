      TYPE Base (l1)
        INTEGER, LEN  :: l1 = 10

        INTEGER :: A01(l1) = -1
      END TYPE

      INTEGER :: J
      INTEGER, PARAMETER :: N = 10 !<-- If N is a constant, tc passes
      TYPE(Base(10)) :: b1(2)

      J = 10
      b1 = [Base(J) :: Base(J)(), Base(J)(3)]
      !b1 = [Base(N) :: Base(N)(), Base(N)(3)]  !<--- this line does not produce an ICE
END
