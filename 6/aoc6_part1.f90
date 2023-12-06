program aoc6_part1
  implicit none

  integer :: time(4), distance(4)

  time = [53, 83, 72, 88]
  distance = [333, 1635, 1289, 1532]

  print *, product(n_winning(time, distance), dim=1)

  contains

    elemental integer function n_winning(time, distance) result (n)
      integer, intent(in) :: time, distance
      integer :: try_time, try_distance
      
      n = 0
      do try_time = 0, time
        try_distance = (time-try_time)*try_time
        if (try_distance>distance) n=n+1
      end do
      
    end function

end program aoc6_part1
