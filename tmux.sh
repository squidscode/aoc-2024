#! /bin/bash

SESSION_NAME="aoc"

# Check if the session already exists
if tmux has-session -t $SESSION_NAME 2>/dev/null; then
    echo "Session $SESSION_NAME already exists. Attaching to it."
    tmux attach-session -t $SESSION_NAME
else
    # Create a new session and name it
    tmux new-session -d -s $SESSION_NAME

    # Send a command to the first pane
    tmux send-keys -t 1 'nvim' C-m

    tmux new-window
    tmux send-keys -t 2 'clear' C-m

    # Attach to the created session
    tmux attach-session -t $SESSION_NAME
fi

