# Example: Uploading Phone Observations with okalaR
# This tutorial shows how to upload field observations using flat dataframes

library(okalaR)

# 1. Authentication --------------------------------------------------------
headers <- auth_headers("your_api_key_here")

# 2. Create Observations Dataframe (flat structure) -----------------------
# Each row is one observation
observations_df <- data.frame(
  feature_uuid = c("feature-001", "feature-001", "feature-001", "feature-002"),
  observation_uuid = c("obs-001", "obs-002", "obs-003", "obs-004"),
  observation_created_at = Sys.time(),
  item_uuid = c("item-text-1", "item-numeric-1", "item-photo-1", "item-label-1"),
  item_type = c("text", "numeric", "phone-photo", "label"),
  observation_value = c("Oak tree", "2.5", "tree_photo.jpg", "123"),
  longitude = c(-0.1276, -0.1276, -0.1276, -0.1280),
  latitude = c(51.5074, 51.5074, 51.5074, 51.5080),
  stringsAsFactors = FALSE
)

# 3. Create Features Dataframe --------------------------------------------
# Each row is one feature (e.g., a survey plot)
features_df <- data.frame(
  feature_uuid = c("feature-001", "feature-002"),
  project_system_id = c(123, 123),
  procedure_id = c(456, 456),
  procedure_start_timestamp = Sys.time(),
  procedure_end_timestamp = Sys.time() + 3600,
  created_by_method = c("drawn", "gps"),
  geometry_type = c("Polygon", "Point"),
  geometry_coords = c(
    # Polygon coordinates (array of arrays)
    '[[[-0.128, 51.507], [-0.127, 51.507], [-0.127, 51.508], [-0.128, 51.508], [-0.128, 51.507]]]',
    # Point coordinates
    '[[-0.128, 51.508]]'
  ),
  stringsAsFactors = FALSE
)

# 4. Create Device Settings -----------------------------------------------
device_df <- data.frame(
  battery_level = 85,
  carrier = "Vodafone",
  build_number = "1.2.3",
  build_id = "build-456",
  device_id = "device-abc-123",
  phone_model = "iPhone 14 Pro",
  phone_operating_system = "iOS 17.1",
  stringsAsFactors = FALSE
)

# 5. Convert to Nested Structure ------------------------------------------
# These helper functions do the heavy lifting
feature_payload <- build_observation_payload(observations_df, features_df)
device_settings <- build_device_settings(device_df)

# 6. Upload WITHOUT Media Files ------------------------------------------
result <- push_phone_observations(
  hdr = headers,
  project_id = 1,
  feature_payload = feature_payload,
  device_settings = device_settings
)

print(result)

# 7. Upload WITH Media Files ---------------------------------------------
# Media files are matched by filename in observation_value column
media_files <- c("path/to/tree_photo.jpg")

result_with_media <- push_phone_observations(
  hdr = headers,
  project_id = 1,
  feature_payload = feature_payload,
  device_settings = device_settings,
  media_files = media_files
)

print(result_with_media)

# 8. Example with Multiple Media Files -----------------------------------
observations_with_media <- data.frame(
  feature_uuid = rep("feature-003", 3),
  observation_uuid = c("obs-005", "obs-006", "obs-007"),
  observation_created_at = Sys.time(),
  item_uuid = rep("item-media-1", 3),
  item_type = c("phone-photo", "phone-video", "phone-audio"),
  observation_value = c("photo1.jpg", "video1.mp4", "audio1.m4a"),
  longitude = -0.1276,
  latitude = 51.5074,
  stringsAsFactors = FALSE
)

features_media <- data.frame(
  feature_uuid = "feature-003",
  project_system_id = 123,
  procedure_id = 456,
  procedure_start_timestamp = Sys.time(),
  procedure_end_timestamp = Sys.time() + 600,
  created_by_method = "gps",
  geometry_type = "Point",
  geometry_coords = '[[-0.1276, 51.5074]]',
  stringsAsFactors = FALSE
)

payload_media <- build_observation_payload(observations_with_media, features_media)

result_multi_media <- push_phone_observations(
  hdr = headers,
  project_id = 1,
  feature_payload = payload_media,
  device_settings = device_settings,
  media_files = c("photo1.jpg", "video1.mp4", "audio1.m4a")
)

# 9. Working with Label Observations -------------------------------------
# Labels are stored as integer IDs
label_observations <- data.frame(
  feature_uuid = "feature-004",
  observation_uuid = "obs-008",
  observation_created_at = Sys.time(),
  item_uuid = "item-species-1",
  item_type = "label",
  observation_value = "12345",  # This is a label_id from the database
  longitude = -0.1276,
  latitude = 51.5074,
  stringsAsFactors = FALSE
)

# You can get label IDs using getIUCNLabels() or get_project_labels()
available_labels <- get_project_labels(headers, labeltype = "Camera")
print(head(available_labels))
