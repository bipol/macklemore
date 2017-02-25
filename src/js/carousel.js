import $ from "jquery";
import slick from "kenwheeler/slick";

class Carousel {
    constructor() {
        Carousel.settings = {
            slidesToShow: 1,
            infinite: false,
            arrows: false,
            adaptiveHeight: true,
            speed: 100,
        };

        this.activitySelectedCount = 0;

        $(document).ready((e) => {
            this.el = $('.carousel');
            this.nextBtn = this.el.find('.carousel__next-btn');
            this.el.slick(Carousel.settings);
            this.setupEvents();
        });


    }

    setupEvents() {
        let self = this;
        $('.activities__btn').on('click', function(e) {
            let btn = $(this);


            self.el.trigger('activity.select', btn.data('kw'));


            if (btn.hasClass('selected')) {
                btn.removeClass('selected');
                self.activitySelectedCount--;

            } else {
                btn.addClass('selected');
                self.activitySelectedCount++;
            }

            switch (self.activitySelectedCount) {

                case 0:
                    self.nextBtn.attr('disabled', 'disabled');
                    break;
                case 1:
                    self.nextBtn.removeAttr('disabled');
            }


        });

        this.nextBtn.on('click', (e) => {
            this.el.slick('slickNext');
        });


    }

    updateProgress() {

    }


}

export default new Carousel();
