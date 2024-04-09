# Get the names of all objects in the environment
all_objects <- ls()

# Remove all objects except "df"
objects_to_remove <- setdiff(all_objects, c("part1", "part2", "part3", "part4", "part5", "part6", "genes", "bigdata"))
rm(list = objects_to_remove)
