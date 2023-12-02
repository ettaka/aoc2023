program aoc2_part2
  implicit none
  character(156) :: line
  integer :: stat, answer=0
  integer :: min_counts(3) !rgb

  open(10, file='input.txt', status='old', iostat=stat)

  main_loop: do while (.true.)
    block
      integer :: i, game_number=0, games_start=0
      character(10) :: ch

      read(10, '(A)', end=99) line

      game_loop: do i=1,len_trim(line)
        if (line(i:i) .eq. ":") then
          games_start = i+1
          read (line(1:i-1),'(A5,I3)') ch, game_number
          exit game_loop
        end if
      end do game_loop

      !print *, "line:", line
      !print *, "game", game_number

      block
        integer :: n_cubes

        n_cubes = 0
        min_counts=0

        min_count_loop: do i=games_start, len_trim(line)-1
          if (line(i:i+2) .eq. 'red') then
            read (line(i-3:i-1), '(I2)') n_cubes
            if (n_cubes.gt.min_counts(1)) min_counts(1) = n_cubes
          else if (line(i:i) .eq. 'g') then
            read (line(i-3:i-1), '(I2)') n_cubes
            if (n_cubes.gt.min_counts(2)) min_counts(2) = n_cubes
          else if (line(i:i) .eq. 'b') then
            read (line(i-3:i-1), '(I2)') n_cubes
            if (n_cubes.gt.min_counts(3)) min_counts(3) = n_cubes
          end if
        end do min_count_loop
      end block

      !print *, "answer", answer
!      print *, "min_counts:", min_counts
!      print *, "product(min_counts, dim=1)", product(min_counts, dim=1)
      answer = answer + product(min_counts, dim=1)
    end block
  end do main_loop

  99 continue
  close(10)

  print *, answer
end program aoc2_part2
