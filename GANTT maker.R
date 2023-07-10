install.packages("ggplot2")
library(ggplot2)

# Function to create a data frame from task specifications
create_task_df <- function(name, start, end) {
  data.frame(name = name, start = start, end = end, stringsAsFactors = FALSE)
}

# Define tasks using 'task name', 'start month', and 'end month'
# Order tasks as you want them to appear on the chart (i.e. by start date and name)
tasks <- list(
  create_task_df("Protocol, database and CRFs", 1, 2),
  create_task_df("HRA and REC approvals", 2, 3),
  create_task_df("Site set-up", 4, 13),
  create_task_df("Recruitment", 4, 21),
  create_task_df("Follow-up", 4, 33),
  create_task_df("PPI group meetings", 1, 36),
  create_task_df("Data cleaning and analysis", 33, 36),
  create_task_df("Final report and dissemination", 35, 36)
)

# Combine tasks into one data frame
task_df <- do.call(rbind, tasks)

# Number of tasks
n_tasks <- nrow(task_df)

# Check if tasks are ordered by start date
if (!is.unsorted(task_df$start)) {
  # Reverse levels for y-axis ordering
  task_df$name <- factor(task_df$name, levels = rev(task_df$name))
} else {
  # If not ordered by start date, reorder tasks and print message for each task that was moved
  original_order <- task_df$name
  task_df$name <- factor(task_df$name, levels = rev(task_df$name[order(task_df$start)]))
  moved_tasks <- setdiff(original_order, task_df$name)
  
  if (length(moved_tasks) > 0) {
    message(paste("The following tasks were re-ordered based on their start date: ", paste(moved_tasks, collapse = ", ")))
  }
}

# Determine the maximum end date for scaling the x-axis
max_end_date <- max(task_df$end)

# Create Gantt chart
g <- ggplot(task_df, aes(fill = name)) +
  geom_rect(aes(xmin = start, xmax = end, ymin = as.numeric(name) - 2.5 / n_tasks, ymax = as.numeric(name) + 0.5 / n_tasks)) +
  scale_y_continuous(labels = levels(task_df$name), breaks = 1:nlevels(task_df$name)) +
  scale_x_continuous(breaks = seq(0, max_end_date, by = 5), limits = c(0, max_end_date + 5)) +
  labs(title = "Project Gantt Chart", x = "Months", y = "", fill = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12), legend.position = "none") +
  scale_fill_brewer(palette = "Set1")

# Save to PDF, dimensions for A4 landscape (dimensions in points)
ggsave("gantt_chart.pdf", plot = g, width = 11.69, height = 8.27, units = "in")

