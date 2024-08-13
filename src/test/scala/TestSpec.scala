import org.mockito.Mockito.when
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.mockito.MockitoSugar
import org.scalatest._
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.matchers.should._

import java.sql.Timestamp


case class MpItemsModel(model: Object, source: String)

package object MpItemsImplicits {
  implicit class MpItemsModelExtensions(val mpItemsModelList: List[MpItemsModel]) {
    def toMarketplaceItemEntity(): Option[MpItemsModel] = {
      if (mpItemsModelList.isEmpty) {
        return None
      }
      else {
        return mpItemsModelList.headOption
      }
    }
  }

  case class MetaData(processedTimestamp: Timestamp)

  case class OverrideFloor(price: Double, expirationDays: Int)

  case class MarketPriceEntry(price: Double, expirationDays: Option[Int], processedAt: Option[Timestamp])

  class TestSpec extends AnyFlatSpec with MockitoSugar {

    def getMarketFloor(eventMetadata: MetaData, floor: OverrideFloor): MarketPriceEntry = {
      MarketPriceEntry(
        price = floor.price,
        expirationDays = Option(floor.expirationDays), processedAt = Option(Timestamp.from(eventMetadata.processedTimestamp.toInstant))

      )
    }

    "MpItem" should "return None when empty" in {
      val mplist: List[MpItemsModel] = List.empty
      val result = mplist.toMarketplaceItemEntity()
      result should be (None)

    }

    "getMarketplaceFloor" should "create a valid entry" in {
      val mockedMetaData = mock[MetaData]
      val mockOverride = mock[OverrideFloor]

      when(mockedMetaData.processedTimestamp)
        .thenReturn(Timestamp.valueOf("2024-08-13 12:00:00"))

      when(mockOverride.price).thenReturn(99.97)

      when(mockOverride.expirationDays).thenReturn(30)

      val result = getMarketFloor(mockedMetaData, mockOverride)

      result.price should be(99.99)
      result.expirationDays should be(Some(30))
      result.processedAt should be(Some(Timestamp.valueOf("2024-08-13 12:00:00")))
    }
  }
}
