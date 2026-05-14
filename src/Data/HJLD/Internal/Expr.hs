{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Data.HJLD.Internal.Expr where
import qualified Data.HJLD.Internal.Kinds as JID
import Data.Text (Text)


data Expr (t :: JID.Type) where
    String :: Text -> Expr 'JID.Value
    -- Node   :: [JID.Proper]
