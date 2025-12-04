import pandas as pd
import time
import os
from supabase import create_client, Client
from dotenv import load_dotenv

# --- CONFIGURATION ---

# Load environment variables from .env file (placed in a higher level or current folder)
load_dotenv() 

# Define constants relying on environment variables
SUPABASE_URL = os.getenv("SUPABASE_URL")
SUPABASE_KEY = os.getenv("SUPABASE_KEY")
TABLE_NAME = "SmartTransit_Integrated"
CSV_FILE = "live_simulation/live_data.csv"  # File path confirmed
BATCH_SIZE = 3
DELAY_SECONDS = 3
CLEANUP_DELAY_SECONDS = 5 

# ----------------------------------------------------------------------------------

def load_data(file_path):
    """Loads and preprocesses the data from the CSV file."""
    if not os.path.exists(file_path):
        print(f"Error: CSV file not found at {file_path}")
        return None
    
    df = pd.read_csv(file_path)
    
    # CRITICAL: Clean column names to match the expected R/Supabase format
    def clean_col_name(col):
        col = str(col).lower().replace(' ', '_').replace('.', '_')
        col = ''.join(e for e in col if e.isalnum() or e == '_')
        return col

    original_cols = df.columns.tolist()
    df.columns = [clean_col_name(col) for col in original_cols]

    # Ensure the required composite keys exist for cleanup logic
    if 'datetime' not in df.columns or 'trip_id' not in df.columns:
        raise KeyError("Required columns 'datetime' and 'trip_id' not found after cleaning. Check your CSV header.")

    print(f"Successfully loaded {len(df)} records from {file_path}")
    print(f"Sample column names after cleaning: {df.columns.tolist()[:10]}...")
    
    return df

def delete_inserted_data(supabase_client: Client, inserted_keys: list):
    """
    Deletes ONLY the records whose composite keys (datetime + trip_id) 
    were inserted in the current cycle.
    """
    if not inserted_keys:
        print("Cleanup skipped: No new records to delete.")
        return

    print(f"\n--- Starting Targeted Cleanup for {len(inserted_keys)} Records ---")
    deleted_count = 0
    
    # We must iterate and delete individually due to Supabase client limitations on composite bulk delete
    for key in inserted_keys:
        try:
            # Delete where datetime equals key['datetime'] AND trip_id equals key['trip_id']
            supabase_client.table(TABLE_NAME) \
                .delete() \
                .eq("datetime", key['datetime']) \
                .eq("trip_id", key['trip_id']) \
                .execute()
            
            deleted_count += 1
            
        except Exception as e:
            print(f"Targeted Cleanup Error for key {key}: {e}")
            
    print(f"Targeted Cleanup successful. Deleted {deleted_count} records.")

def inject_data_to_supabase(data_df: pd.DataFrame, supabase_client: Client):
    """
    Splits the dataframe into batches and uploads them sequentially, 
    returning the list of composite keys (datetime + trip_id) for cleanup.
    """
    total_records = len(data_df)
    # Store composite keys as a list of dictionaries: [{'datetime': val, 'trip_id': val}, ...]
    inserted_keys = []
    print(f"\n--- Starting Real-time Data Injection for {total_records} Records ---")
    
    # Convert DataFrame to a list of dictionaries (JSON records)
    data_list = data_df.to_dict('records')
    
    i = 0
    while i < total_records:
        batch = data_list[i:i + BATCH_SIZE]
        
        try:
            # Upload data to the Supabase table. 
            response = supabase_client.table(TABLE_NAME).insert(batch).execute()
            
            # Record the composite keys of the inserted batch for cleanup
            for record in batch:
                inserted_keys.append({
                    'datetime': record['datetime'],
                    'trip_id': record['trip_id']
                })
            
            if hasattr(response, 'data') and response.data:
                print(f"[{time.strftime('%H:%M:%S')}] Uploaded {len(response.data)} records (Total injected: {i + len(response.data)})")
            else:
                print(f"[{time.strftime('%H:%M:%S')}] Successfully executed batch insert (Count: {len(batch)}).")
            
        except Exception as e:
            print(f"[{time.strftime('%H:%M:%S')}] Supabase Upload Error: {e}")
            
        i += BATCH_SIZE
        
        # Pause to test the R Shiny reactivePoll interval (5s)
        if i < total_records:
            print(f"Waiting {DELAY_SECONDS} seconds...")
            time.sleep(DELAY_SECONDS)
        
    print("\n--- Data Injection Cycle Complete ---")
    return inserted_keys

def main():
    if not SUPABASE_URL or not SUPABASE_KEY:
        print("FATAL ERROR: SUPABASE_URL or SUPABASE_KEY not found. Ensure they are set in your environment or .env file.")
        return

    try:
        data = load_data(CSV_FILE)
    except KeyError as e:
        print(f"Exiting: Data loading failed due to missing column. {e}")
        return

    if data is None or data.empty:
        print("Exiting: No data loaded.")
        return

    try:
        # Initialize Supabase client
        supabase_client = create_client(SUPABASE_URL, SUPABASE_KEY)
        
        print(f"\n====================== Starting Single Injection Cycle ======================")
        
        # 1. Inject the data and get the keys
        keys_to_delete = inject_data_to_supabase(data, supabase_client)
        
        # 2. Cleanup after the run
        print(f"Waiting {CLEANUP_DELAY_SECONDS} seconds for R Shiny final refresh before cleanup...")
        time.sleep(CLEANUP_DELAY_SECONDS)
        delete_inserted_data(supabase_client, keys_to_delete)
        
        print("\n====================== Cycle Complete. Exiting. ======================")

    except Exception as e:
        print(f"\nFATAL CLIENT ERROR: {e}")

if __name__ == "__main__":
    main()