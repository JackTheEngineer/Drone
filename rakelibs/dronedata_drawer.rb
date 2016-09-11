
def draw_data_from_file(filename)
  gnuplot(%Q(
  set terminal svg
  set output "curves.svg"
  plot "#{filename}" every 40 using 1:4 title 'Z-Pos', \
                      "#{filename}" every 40 using 1:7 title 'Z-speed'
))
end


def gnuplot(commands)
  IO.popen("gnuplot", "w") { |io| io.puts commands }
end

