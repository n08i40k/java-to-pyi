use std::io::{self, Write};
use std::sync::{Mutex, OnceLock};

static STATUS_LEN: OnceLock<Mutex<usize>> = OnceLock::new();

fn with_status_len<F: FnOnce(&mut usize)>(func: F) {
    let lock = STATUS_LEN.get_or_init(|| Mutex::new(0));
    if let Ok(mut len) = lock.lock() {
        func(&mut len);
    }
}

pub fn update(message: &str) {
    let new_len = message.len();
    with_status_len(|last_len| {
        let padding = if new_len < *last_len {
            " ".repeat(*last_len - new_len)
        } else {
            String::new()
        };
        let mut stderr = io::stderr();
        let _ = write!(stderr, "\r{}{}", message, padding);
        let _ = stderr.flush();
        *last_len = new_len;
    });
}

pub fn clear() {
    with_status_len(|last_len| {
        if *last_len == 0 {
            return;
        }
        let mut stderr = io::stderr();
        let _ = write!(stderr, "\r{} \r", " ".repeat(*last_len));
        let _ = stderr.flush();
        *last_len = 0;
    });
}
