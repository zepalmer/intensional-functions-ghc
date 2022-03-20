{-# LANGUAGE LambdaCase #-}

foo = f >>= \case
        Just h -> loadTestDB (h ++ "/.testdb")
        Nothing -> fmap S.Right initTestDB

foo = f >>= \cases
        x (Just h) -> loadTestDB (h ++ "/.testdb")
        _ Nothing -> fmap S.Right initTestDB

foo = f >>= \cases
        | a -> loadTestDB (h ++ "/.testdb")
        | b -> fmap S.Right initTestDB

{-| Is the alarm set - i.e. will it go off at some point in the future even if
   `setAlarm` is not called? -}
isAlarmSetSTM :: AlarmClock -> STM Bool
isAlarmSetSTM AlarmClock{..} = readTVar acNewSetting
  >>= \case { AlarmNotSet -> readTVar acIsSet; _ -> return True }
